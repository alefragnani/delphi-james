//-------------------------------------------------------------------------------------------------
//
// Copyright (c) Alessandro Fragnani. All rights reserved
//
// Licensed under the MIT License. See License.md in the project root for license information.
//
// https://github.com/alefragnani/delphi-james
//
//-------------------------------------------------------------------------------------------------

unit DelphiSettings;

interface

uses
  System.Classes,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.Win.Registry,
  System.SysUtils,
  DelphiVersionInfo;

const
  TAG_BASE_FOLDER = '#(JAMESBASEFOLDER)';

type
  EDelphiVersionNotSupported = class(Exception);

  TLogEvent = procedure(Sender: TObject; const msg: string);

  TDelphiSettings = class
  private
    FSupportedVersions: TList<TDelphiVersion>;
    FCurrentVersion: TDelphiVersion;
    FBaseFolder: string;

    FRegistry: System.Win.Registry.TRegistry;
    FKnownPackages: System.Generics.Collections.TList<string>;
    FLibraryPath: array[dpWin32..dpAndroid32] of System.Generics.Collections.TList<string>;
    FEnvironmentVariables: System.Generics.Collections.TList<string>;

    FLastError: string;

    FOnLog: TLogEvent;

    procedure InitSupportedVersions;
    function DoubleBackslash(const path: string): string;
    function RemoveDoubleQuotes(const path: string): string;
    function ReplaceBaseFolderByTag(const path: string): string;
    function ReplaceBaseFolderByRealValue(const path: string): string;

    procedure SetLastError(const operation, message: string);
    function GetLastError: string;

    function GetInstalledVersions: TArray<string>;
    function GetKnownPackages: TArray<string>;
    function GetVersion: string;
    procedure SetVersion(value: string);
    function GetLibraryPath(index: TDelphiPlatform): TArray<string>;
    function GetEnvironmentVariables: TArray<string>;

    procedure ResetSettings;
    procedure LoadKnownPackages;
    procedure LoadLibraryPath;
    procedure LoadEnvironmentVariables;
    procedure SaveKnownPackages;
    procedure SaveLibraryPath;
    procedure SaveEnvironmentVariables;

    procedure Log(const msg: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadDelphiSettings;
    procedure SaveDelphiSettings;
    function LoadDelphiSettingsFromJSON(const filename: string): boolean;
    procedure SaveDelphiSettingsToJSON(const filename: string);

    property BaseFolder: string read FBaseFolder write FBaseFolder;
    property Version: string read GetVersion write SetVersion;
    property InstalledVersions: TArray<string> read GetInstalledVersions;

    property KnownPackages: TArray<string> read GetKnownPackages;
    property LibraryPath[index: TDelphiPlatform]: TArray<string> read GetLibraryPath;
    property EnvironmentVariables: TArray<string> read GetEnvironmentVariables;

    property LastError: string read GetLastError;
    property OnLog: TLogEvent read FOnLog write FOnLog;
  end;

implementation

uses
  System.JSON,
  System.JSON.Builders,
  System.JSON.Writers,
  System.Variants,
  Converters;



constructor TDelphiSettings.Create;
begin
  FLastError := '';

  FCurrentVersion := nil;
  FRegistry := TRegistry.Create;

  FSupportedVersions := TList<TDelphiVersion>.Create;
  InitSupportedVersions;
end;

destructor TDelphiSettings.Destroy;
var
  i: byte;
begin
  if Assigned(FKnownPackages) then
  begin
    FKnownPackages.Clear;
    FKnownPackages.Free;

    for i := Ord(Low(FLibraryPath)) to Ord(High(FLibraryPath)) do
    begin
      FLibraryPath[TDelphiPlatform(i)].Clear;
      FLibraryPath[TDelphiPlatform(i)].Free;
    end;
    FEnvironmentVariables.Clear;
    FEnvironmentVariables.Free;
  end;
end;

procedure TDelphiSettings.InitSupportedVersions;
begin
  FSupportedVersions.Add(TDelphi5.Create);
  FSupportedVersions.Add(TDelphi2006.Create);
  FSupportedVersions.Add(TDelphiSeattle.Create);
  FSupportedVersions.Add(TDelphiBerlin.Create);
  FSupportedVersions.Add(TDelphiTokyo.Create);
  FSupportedVersions.Add(TDelphiRio.Create);
end;

function TDelphiSettings.GetInstalledVersions: TArray<string>;
var
  supported: TList<string>;
  version: TDelphiVersion;
begin
  supported := TList<string>.Create;
  try
    for version in FSupportedVersions do
    begin
      if FRegistry.KeyExists(version.RegistryKey) then
        supported.Add(version.Description);
    end;
    result := supported.ToArray;
  finally
    supported.Free;
  end;
end;

function TDelphiSettings.GetKnownPackages: TArray<string>;
begin
  result := FKnownPackages.ToArray;
end;

function TDelphiSettings.GetLibraryPath(index: TDelphiPlatform): TArray<string>;
begin
  result := FLibraryPath[index].ToArray;
end;

function TDelphiSettings.GetEnvironmentVariables: TArray<string>;
begin
  result := FEnvironmentVariables.ToArray;
end;

function TDelphiSettings.GetVersion: string;
begin
  if Assigned(FCurrentVersion) then
    result := FCurrentVersion.Description
  else
    result := '';
end;


procedure TDelphiSettings.SetVersion(value: string);
var
  version: TDelphiVersion;
  installed: boolean;
  iv: string;
begin
  if Assigned(FCurrentVersion) and (FCurrentVersion.Description.ToLower = value.ToLower) then
    exit;

  ResetSettings;
  FCurrentVersion := nil;

  for version in FSupportedVersions do
  begin
    if version.Description.ToLower = value.ToLower then
    begin
      FCurrentVersion := version;
      break;
    end;
  end;

  if FCurrentVersion = nil then
    raise EDelphiVersionNotSupported.CreateFmt('Version "%s" is not supported', [value]);

  installed := False;
  for iv in InstalledVersions do
  begin
    if iv.ToLower = FCurrentVersion.Description.ToLower then
    begin
      installed := True;
      break;
    end;
  end;

  if not installed then
    raise EDelphiVersionNotSupported.CreateFmt('Version "%s" is not installed', [value]);

end;


procedure TDelphiSettings.LoadKnownPackages;
var
  oKeys: TStringList;
  key: string;
begin
  if not FRegistry.OpenKey(FCurrentVersion.RegistryKeyKnownPackages, False) then
    exit;

  Log('- Known Packages');
  oKeys := TStringList.Create;
  try
    FRegistry.GetValueNames(oKeys);
    Log(Format('  %d Packages detected', [oKeys.Count]));
    for key in oKeys do
    begin
      FKnownPackages.Add(key);
    end;
  finally
    oKeys.Free;
  end;
  FRegistry.CloseKey;
end;

procedure TDelphiSettings.LoadLibraryPath;
var
  oKeys: TStringList;
  key, searchPath: string;
  i: byte;
begin
  Log('- Library Path');
  for i := Ord(Low(FLibraryPath)) to Ord(High(FLibraryPath)) do
  begin
    if not FRegistry.OpenKey(FCurrentVersion.GetLibraryPathForPlatform(TDelphiPlatform(i)), False) then
      Continue;

    Log('  - ' + TDelphiVersion.PlatformStr(TDelphiPlatform(i)));
    oKeys := TStringList.Create;
    try
      searchPath := FRegistry.ReadString('Search Path');
      if searchPath <> '' then
      begin
        oKeys := TStringList.Create;
        oKeys.Delimiter := ';';
        oKeys.StrictDelimiter := true;
        oKeys.DelimitedText := searchPath;

        Log(Format('    %d paths loaded', [oKeys.Count]));
        for key in oKeys do
        begin
          FLibraryPath[TDelphiPlatform(i)].Add(key);
        end;
      end;
//      else
//        Log('    NO paths');
    finally
      oKeys.Free;
    end;
    FRegistry.CloseKey;
  end;
end;

procedure TDelphiSettings.LoadEnvironmentVariables;
var
  oKeys: TStringList;
  key, searchPath: string;
begin
  if not FRegistry.OpenKey(FCurrentVersion.RegistryKeyEnvironmentVariables, False) then
    exit;

  Log('- Environment Variables');
  oKeys := TStringList.Create;
  try
    searchPath := FRegistry.ReadString('PATH');
    if searchPath <> '' then
    begin
      oKeys := TStringList.Create;
      oKeys.Delimiter := ';';
      oKeys.StrictDelimiter := true;
      oKeys.DelimitedText := searchPath;

      Log(Format('  %d PATH variables loaded', [oKeys.Count]));
      for key in oKeys do
      begin
        FEnvironmentVariables.Add(key);
      end;
    end;
//    else
//      Log('  NO PATH variables');
  finally
    oKeys.Free;
  end;
  FRegistry.CloseKey;
end;

procedure TDelphiSettings.ResetSettings;
var
  i: byte;
begin
  if not Assigned(FKnownPackages) then
    Exit;

  FKnownPackages.Clear;
  for i := Ord(Low(FLibraryPath)) to Ord(High(FLibraryPath)) do
    FLibraryPath[TDelphiPlatform(i)].Clear;
  FEnvironmentVariables.Clear;
end;

procedure TDelphiSettings.LoadDelphiSettings;
var
  i: byte;
begin
  if not Assigned(FCurrentVersion) then
    exit;

  if not Assigned(FKnownPackages) then
  begin
    FKnownPackages := TList<string>.Create;
    for i := Ord(Low(TDelphiPlatform)) to Ord(High(TDelphiPlatform)) do
    begin
        FLibraryPath[TDelphiPlatform(i)] := TList<string>.Create;
    end;
    FEnvironmentVariables := TList<string>.Create;
  end;

  LoadKnownPackages;
  LoadLibraryPath;
  LoadEnvironmentVariables;
end;


{#REGION Save}

procedure TDelphiSettings.SaveKnownPackages;
var
  oKeys: TStringList;
  package: string;
begin
  if not FRegistry.OpenKey(FCurrentVersion.RegistryKeyKnownPackages, False) then
    exit;

  Log('- Known Packages');
  oKeys := TStringList.Create;
  try
    FRegistry.GetValueNames(oKeys);
    for package in oKeys do
    begin
      FRegistry.DeleteValue(package);
    end;
  finally
    oKeys.Free;
  end;

  if FKnownPackages.Count = 0 then
    exit;

  Log(Format('  %d packages applied', [FKnownPackages.Count]));
  for package in FKnownPackages do
  begin
    FRegistry.WriteString(package, package);
  end;
  FRegistry.CloseKey;
end;

procedure TDelphiSettings.SaveLibraryPath;
var
  path: string;
  sb: TStringBuilder;
  i: byte;
begin
  Log('- Library Path');
  for i := Ord(Low(FLibraryPath)) to Ord(High(FLibraryPath)) do
  begin
    if not FRegistry.OpenKey(FCurrentVersion.GetLibraryPathForPlatform(TDelphiPlatform(i)), False) then
      Continue;

    Log('  - ' + TDelphiVersion.PlatformStr(TDelphiPlatform(i)));
    if FLibraryPath[TDelphiPlatform(i)].Count > 0 then
//      FRegistry.WriteString('Search Path', '')
//    else
    begin
      Log(Format('    %d paths applied', [FLibraryPath[TDelphiPlatform(i)].Count]));
      sb := TStringBuilder.Create;
      for path in FLibraryPath[TDelphiPlatform(i)] do
      begin
        sb.Append(path);
        sb.Append(';');
      end;
      sb.Remove(sb.Length - 1, 1);
      FRegistry.WriteString('Search Path', sb.ToString());
    end;
//    else
//      Log('    NO PATH applied');
    FRegistry.CloseKey;
  end;
end;

procedure TDelphiSettings.SaveEnvironmentVariables;
var
  sb: TStringBuilder;
  path: string;
begin
  if not FRegistry.OpenKey(FCurrentVersion.RegistryKeyEnvironmentVariables, False) then
    exit;

  Log('- Environment Variables');
  if FEnvironmentVariables.Count = 0 then
    FRegistry.WriteString('PATH', '')
  else
  begin
    Log(Format('    %d PATH applied', [FEnvironmentVariables.Count]));
    sb := TStringBuilder.Create;
    for path in FEnvironmentVariables do
    begin
      sb.Append(path);
      sb.Append(';');
    end;
    sb.Remove(sb.Length - 1, 1);
    FRegistry.WriteString('PATH', sb.ToString());
  end;
  FRegistry.CloseKey;
end;

procedure TDelphiSettings.SaveDelphiSettings;
begin
  SaveKnownPackages;
  SaveLibraryPath;
  SaveEnvironmentVariables;
end;

function TDelphiSettings.LoadDelphiSettingsFromJSON(const filename: string): boolean;
var
  sl: TStringList;
  o, olp: TJSONObject;
  a: TJSONArray;
  idx: integer;
  i: byte;
begin
  if not Assigned(FKnownPackages) then
  begin
    FKnownPackages := TList<string>.Create;
    for i := Ord(Low(TDelphiPlatform)) to Ord(High(TDelphiPlatform)) do
    begin
        FLibraryPath[TDelphiPlatform(i)] := TList<string>.Create;
    end;
    FEnvironmentVariables := TList<string>.Create;
  end;

  sl := TStringList.Create;
  try
    sl.LoadfromFile(filename);

    o := TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(sl.Text),0) as TJSONObject;
    if o = nil then
    begin
      result := False;
      SetLastError('Loading Settings from file', 'Invalid JSON file. Pleas check if it is a valid JSON file');
      Exit;
    end;

    try
      Version := o.GetValue('version').Value ;
      BaseFolder := ExtractFileDir(filename);

      a := TJSONArray(o.Get('known_packages').JsonValue);
      for idx := 0 to a.Count - 1 do
        FKnownPackages.Add(ReplaceBaseFolderByRealValue(RemoveDoubleQuotes(a.Items[idx].ToString)));

      if not FCurrentVersion.SupportsVariousPlatforms then
      begin
        a := TJSONArray(o.Get('library_path').JsonValue);
        for idx := 0 to a.Count - 1 do
          FLibraryPath[dpWin32].Add(ReplaceBaseFolderByRealValue(RemoveDoubleQuotes(a.Items[idx].ToString)));
      end
      else
      begin
        olp := TJSONObject(o.Get('library_path').JsonValue);
        for i := Ord(Low(TDelphiPlatform)) to Ord(High(TDelphiPlatform)) do
        begin
          // exists?
          if olp.Get(AnsiLowerCase(TDelphiVersion.PlatformStr(TDelphiPlatform(i)))) <> nil then
          begin
            a := TJSONArray(olp.Get(AnsiLowerCase(TDelphiVersion.PlatformStr(TDelphiPlatform(i)))).JsonValue);
            for idx := 0 to a.Count - 1 do
              FLibraryPath[TDelphiPlatform(i)].Add(ReplaceBaseFolderByRealValue(RemoveDoubleQuotes(a.Items[idx].ToString)));
          end;
        end;
      end;

      a := TJSONArray(o.Get('environment_variables').JsonValue);
      for idx := 0 to a.Count - 1 do
        FEnvironmentVariables.Add(ReplaceBaseFolderByRealValue(RemoveDoubleQuotes(a.Items[idx].ToString)));

      result := True;
    finally
      o.Free;
    end;
  finally
    sl.free;
  end;
end;

function TDelphisettings.ReplaceBaseFolderByTag(const path: string): string;
begin
  result := StringReplace(path, BaseFolder, TAG_BASE_FOLDER, [rfIgnoreCase]);
end;

function TDelphisettings.ReplaceBaseFolderByRealValue(const path: string): string;
begin
  result := StringReplace(path, TAG_BASE_FOLDER, BaseFolder, [rfIgnoreCase]);
end;

function TDelphiSettings.DoubleBackslash(const path: string): string;
begin
  {$IF CompilerVersion < 33}
  result := StringReplace(path, '\', '\\', [rfReplaceAll]);
  {$ELSE}
  result := path;
  {$ENDIF}
end;

function TDelphiSettings.RemoveDoubleQuotes(const path: string): string;
begin
  result := path;
  if result[1] = '"' then
    result := Copy(result, 2, MaxInt);
  if result[length(result)] = '"' then
    result := Copy(result, 1, Length(result) - 1);
end;

procedure TDelphiSettings.SetLastError(const operation, message: string);
begin
  FLastError := Format('[%s]: %s', [operation, message]);
end;

function TDelphiSettings.GetLastError: string;
begin
  result := FLastError;
end;

procedure TDelphiSettings.SaveDelphiSettingsToJSON(const filename: string);
var
  o, olp: TJSONObject;
  a: TJSONArray;
  item: string;
  sl: TStringList;
  i: byte;
begin
  o := TJSONObject.Create;
  try
    // add the array to the object.
    o.AddPair('version', FCurrentVersion.Description);
    BaseFolder := DoubleBackslash(ExtractFileDir(filename));

    //
    a := TJSONArray.Create();
    o.AddPair('known_packages', a);
    for item in FKnownPackages do
      a.Add(ReplaceBaseFolderByTag(DoubleBackslash(item)));

    if not FCurrentVersion.SupportsVariousPlatforms then
    begin
      a := TJSONArray.Create();
      o.AddPair('library_path', a);
      for item in FLibraryPath[dpWin32] do
        a.Add(ReplaceBaseFolderByTag(DoubleBackslash(item)));
    end
    else
    begin
      olp := TJSONObject.Create;
      for i := Ord(Low(TDelphiPlatform)) to Ord(High(TDelphiPlatform)) do
      begin
        if FLibraryPath[TDelphiPlatform(i)].Count > 0 then
        begin
          a := TJSONArray.Create();
          olp.AddPair(AnsiLowerCase(TDelphiVersion.PlatformStr(TDelphiPlatform(i))), a);
          for item in FLibraryPath[TDelphiPlatform(i)] do
            a.Add(ReplaceBaseFolderByTag(DoubleBackslash(item)));
        end;
      end;
      o.AddPair('library_path', olp);
    end;

    a := TJSONArray.Create();
    o.AddPair('environment_variables', a);
    for item in FEnvironmentVariables do
      a.Add(ReplaceBaseFolderByTag(DoubleBackslash(item)));

  finally
    sl := TStringList.Create;
    sl.Text := TConverters.jsonReformat(o.ToString, True);
    sl.SaveToFile(filename);
    sl.free;
    o.Free;
  end;
end;

procedure TDelphiSettings.Log(const msg: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, msg);
end;

{#ENDREGION }

end.
