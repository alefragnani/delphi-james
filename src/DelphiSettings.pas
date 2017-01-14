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

  TDelphiSettings = class
  private
    FSupportedVersions: TList<TDelphiVersionInfo>;
    FCurrentVersion: TDelphiVersionInfo;
    FBaseFolder: string;

    FRegistry: System.Win.Registry.TRegistry;
    FKnownPackages: System.Generics.Collections.TList<string>;
    FLibraryPath: System.Generics.Collections.TList<string>;
    FEnvironmentVariables: System.Generics.Collections.TList<string>;

    procedure InitSupportedVersions;
    function DoubleBackslash(const path: string): string;
    function RemoveDoubleQuotes(const path: string): string;
    function ReplaceBaseFolderByTag(const path: string): string;
    function ReplaceBaseFolderByRealValue(const path: string): string;

    function GetInstalledVersions: TArray<string>;
    function GetKnownPackages: TArray<string>;
    function GetVersion: string;
    procedure SetVersion(value: string);
    function GetLibraryPath: TArray<string>;
    function GetEnvironmentVariables: TArray<string>;

    procedure ResetSettings;
    procedure LoadKnownPackages;
    procedure LoadLibraryPath;
    procedure LoadEnvironmentVariables;
    procedure SaveKnownPackages;
    procedure SaveLibraryPath;
    procedure SaveEnvironmentVariables;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadDelphiSettings;
    procedure SaveDelphiSettings;
    procedure LoadDelphiSettingsFromJSON(const filename: string);
    procedure SaveDelphiSettingsToJSON(const filename: string);

    property BaseFolder: string read FBaseFolder write FBaseFolder;
    property Version: string read GetVersion write SetVersion;
    property InstalledVersions: TArray<string> read GetInstalledVersions;

    property KnownPackages: TArray<string> read GetKnownPackages;
    property LibraryPath: TArray<string> read GetLibraryPath;
    property EnvironmentVariables: TArray<string> read GetEnvironmentVariables;
  end;

implementation

uses
  System.JSON,
  System.JSON.Builders,
  System.JSON.Writers,
  Converters;



constructor TDelphiSettings.Create;
begin
  FCurrentVersion := nil;
  FKnownPackages := TList<string>.Create;
  FLibraryPath := TList<string>.Create;
  FEnvironmentVariables := TList<string>.Create;
  FRegistry := TRegistry.Create;

  FSupportedVersions := TList<TDelphiVersionInfo>.Create;

  InitSupportedVersions;
end;

destructor TDelphiSettings.Destroy;
begin
  FKnownPackages.Clear;
  FKnownPackages.Free;
  FLibraryPath.Clear;
  FLibraryPath.Free;
  FEnvironmentVariables.Clear;
  FEnvironmentVariables.Free;
end;

procedure TDelphiSettings.InitSupportedVersions;
begin
  FSupportedVersions.Add(TDelphiVersionInfo.Create('Delphi 2006',
    'Software\Borland\BDS\4.0',
    'Software\Borland\BDS\4.0\Library',
    ''));
  FSupportedVersions.Add(TDelphiVersionInfo.Create('Delphi Berlin',
    'Software\Embarcadero\BDS\18.0',
    'Software\Embarcadero\BDS\18.0\Library\Win32',
    'Software\Embarcadero\BDS\18.0\Environment Variables'));
end;

function TDelphiSettings.GetInstalledVersions: TArray<string>;
var
  supported: TList<string>;
  version: TDelphiVersionInfo;
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

function TDelphiSettings.GetLibraryPath: TArray<string>;
begin
  result := FLibraryPath.ToArray;
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
  version: TDelphiVersionInfo;
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
  if not FRegistry.OpenKey(FCurrentVersion.RegistryKey + '\Known Packages', False) then
    exit;

  oKeys := TStringList.Create;
  try
    FRegistry.GetValueNames(oKeys);
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
begin
  if not FRegistry.OpenKey(FCurrentVersion.RegistryKeyLibraryPath, False) then
    exit;

  oKeys := TStringList.Create;
  try
    searchPath := FRegistry.ReadString('Search Path');
    if searchPath <> '' then
    begin
      oKeys := TStringList.Create;
      oKeys.Delimiter := ';';
      oKeys.StrictDelimiter := true;
      oKeys.DelimitedText := searchPath;

      for key in oKeys do
      begin
        FLibraryPath.Add(key);
      end;
    end;
  finally
    oKeys.Free;
  end;
  FRegistry.CloseKey;
end;

procedure TDelphiSettings.LoadEnvironmentVariables;
var
  oKeys: TStringList;
  key, searchPath: string;
begin
  if not FRegistry.OpenKey(FCurrentVersion.RegistryKeyEnvironmentVariables, False) then
    exit;

  oKeys := TStringList.Create;
  try
    searchPath := FRegistry.ReadString('PATH');
    if searchPath <> '' then
    begin
      oKeys := TStringList.Create;
      oKeys.Delimiter := ';';
      oKeys.StrictDelimiter := true;
      oKeys.DelimitedText := searchPath;

      for key in oKeys do
      begin
        FEnvironmentVariables.Add(key);
      end;
    end;
  finally
    oKeys.Free;
  end;
  FRegistry.CloseKey;
end;

procedure TDelphiSettings.ResetSettings;
begin
  FKnownPackages.Clear;
  FLibraryPath.Clear;
  FEnvironmentVariables.Clear;
end;

procedure TDelphiSettings.LoadDelphiSettings;
begin
  if not Assigned(FCurrentVersion) then
    exit;

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
  if not FRegistry.OpenKey(FCurrentVersion.RegistryKey + '\Known Packages', False) then
    exit;

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
begin
  if not FRegistry.OpenKey(FCurrentVersion.RegistryKeyLibraryPath, False) then
    exit;

  if FLibraryPath.Count = 0 then
    FRegistry.WriteString('Search Path', '')
  else
  begin
    sb := TStringBuilder.Create;
    for path in FLibraryPath do
    begin
      sb.Append(path);
      sb.Append(';');
    end;
    sb.Remove(sb.Length - 1, 1);
    FRegistry.WriteString('Search Path', sb.ToString());
  end;
  FRegistry.CloseKey;
end;

procedure TDelphiSettings.SaveEnvironmentVariables;
var
  sb: TStringBuilder;
  path: string;
begin
  if not FRegistry.OpenKey(FCurrentVersion.RegistryKeyEnvironmentVariables, False) then
    exit;

  if FEnvironmentVariables.Count = 0 then
    FRegistry.WriteString('PATH', '')
  else
  begin
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

procedure TDelphiSettings.LoadDelphiSettingsFromJSON(const filename: string);
var
  sl: TStringList;
  o: TJSONObject;
  a: TJSONArray;
  idx: integer;
begin
  sl := TStringList.Create;
  try
    sl.LoadfromFile(filename);

    o := TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(sl.Text),0) as TJSONObject;
    try
      Version := o.GetValue('version').Value ;
      BaseFolder := ExtractFileDir(filename);

      a := TJSONArray(o.Get('known_packages').JsonValue);
      for idx := 0 to a.Count - 1 do
        FKnownPackages.Add(ReplaceBaseFolderByRealValue(RemoveDoubleQuotes(a.Items[idx].ToString)));

      a := TJSONArray(o.Get('library_path').JsonValue);
      for idx := 0 to a.Count - 1 do
        FLibraryPath.Add(ReplaceBaseFolderByRealValue(RemoveDoubleQuotes(a.Items[idx].ToString)));

      a := TJSONArray(o.Get('environment_variables').JsonValue);
      for idx := 0 to a.Count - 1 do
        FEnvironmentVariables.Add(ReplaceBaseFolderByRealValue(RemoveDoubleQuotes(a.Items[idx].ToString)));
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
  result := StringReplace(path, '\', '\\', [rfReplaceAll]);
end;

function TDelphiSettings.RemoveDoubleQuotes(const path: string): string;
begin
  result := path;
  if result[1] = '"' then
    result := Copy(result, 2, MaxInt);
  if result[length(result)] = '"' then
    result := Copy(result, 1, Length(result) - 1);
end;

procedure TDelphiSettings.SaveDelphiSettingsToJSON(const filename: string);
var
  o: TJSONObject;
  a: TJSONArray;
  item: string;
  sl: TStringList;
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

    a := TJSONArray.Create();
    o.AddPair('library_path', a);
    for item in FLibraryPath do
      a.Add(ReplaceBaseFolderByTag(DoubleBackslash(item)));

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

{#ENDREGION }

end.
