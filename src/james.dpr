//-------------------------------------------------------------------------------------------------
//
// Copyright (c) Alessandro Fragnani. All rights reserved
//
// Licensed under the MIT License. See License.md in the project root for license information.
//
// https://github.com/alefragnani/delphi-james
//
//-------------------------------------------------------------------------------------------------

program james;

{$APPTYPE CONSOLE}

{$R *.res}

/// Supported Parameters
///
///   Loading Settings
///     -l = Load Delphi Settings (which Delphi version)
///
///   Applying Settings
///     -a = Apply Delphi Settings (.james file path)

uses
  System.SysUtils,
  Vcl.Forms,
  DelphiSettings in 'DelphiSettings.pas',
  DelphiVersionInfo in 'DelphiVersionInfo.pas',
  Converters in '..\utils\Converters.pas',
  Writers in '..\utils\Writers.pas';

  procedure WriteHeader;
  begin
    WriteLn('');
    WriteLn('****************************');
    WriteLn('** Delphi James v.0.4.0.0 **');
    WriteLn('****************************');
    WriteLn('');
  end;

  procedure WriteParametersHelp;
  begin
    WriteLn(' James [options]:[params]');
    WriteLn('');
    Writeln(' Available options: ');
    WriteLn('  -l   Load Delphi Settings and save to .james file');
    Writeln('  -a   Apply Delphi Settings stored in .james file');
    WriteLn('');
    Writeln(' Delphi Supported Versions: ');
    WriteLn('  - 5');
    Writeln('  - 2006');
    Writeln('  - Seattle');
    Writeln('  - Berlin');
  end;

  procedure WriteHelp;
  begin
    WriteHeader;
    WriteParametersHelp;
  end;

  procedure WriteParameterError(const msg: string);
  begin
    WriteHeader;
    WriteLn(msg);
    WriteLn('');
    WriteParametersHelp ;
  end;

  procedure WriteCommandStart(const msg: string);
  begin
    WriteHeader;
    WriteLn('.. ' + msg + ' ..');
  end;

  procedure WriteCommandEnd(const msg: string);
  begin
    WriteLn('.. Finished ..');
    WriteLn('  -> ' + msg);
  end;

  procedure WriteCommandError(const msg: string);
  begin
    WriteLn(' ERROR: ' + msg);
  end;

  procedure Log(Sender: TObject; const msg: string);
  begin
    WriteLn('   ' + msg);
  end;

var
  FDelphiSettings: TDelphiSettings;
  param1, jamesFile: string;
begin
  // -h (HELP)
  if FindCmdLineSwitch('h', param1, true, [clstValueAppended]) then
  begin
    WriteHelp;
    Exit;
  end;

  // -l (LOAD)
  if FindCmdLineSwitch('l', param1, true, [clstValueAppended]) then
  begin
    if param1 = '' then
    begin
      WriteParameterError('Error: No version defined. Use -l:VERSION');
      exit;
    end;

    WriteCommandStart('Loading Delphi Settings');
    jamesFile := IncludeTrailingPathDelimiter(GetCurrentDir) + '.james';
    FDelphiSettings := TDelphiSettings.Create;
    try
      try
        if FindCmdLineSwitch('v') then
          FDelphiSettings.OnLog := Log;

        FDelphiSettings.Version := 'Delphi ' + param1;
        FDelphiSettings.LoadDelphiSettings;
        FDelphiSettings.SaveDelphiSettingsToJSON(jamesFile);
        WriteCommandEnd('Settings saved to .james file');
      except
        on e: EDelphiVersionNotSupported do
          WriteLn('Error: ' + e.Message);
      end;
    finally
      FDelphiSettings.Free;
    end;
    Exit;
  end;

  // no params, look for '.james' file to 'apply'
  if ParamCount = 0 then
  begin
    param1 := IncludeTrailingPathDelimiter(GetCurrentDir) + '.james';
    if not FileExists(param1) then
    begin
      WriteParameterError('Error: File ".james" does not exists.');
      Exit;
    end;
  end
  else
  begin
    //param1 := ParamStr(1);
    if not FindCmdLineSwitch('a', param1, true, [clstValueAppended]) then
    begin
      WriteParameterError('Error: Invalid parameter.');
      Exit;
    end;

    param1 := ExpandFileName(param1);
    if not FileExists(param1) then
    begin
      WriteParameterError('Error: File "' + param1 + '" does not exists.');
      Exit;
    end;
  end;

  WriteCommandStart('Applying Delphi Settings');
  FDelphiSettings := TDelphiSettings.Create;
  try
    if FindCmdLineSwitch('v') then
      FDelphiSettings.OnLog := Log;
    if FDelphiSettings.LoadDelphiSettingsFromJSON(param1) then
    begin
      FDelphiSettings.SaveDelphiSettings;
      WriteCommandEnd('Settings applied to ' + FDelphiSettings.Version);
    end
    else
      WriteCommandError('Error: ' + FDelphiSettings.LastError);
  finally
    FDelphiSettings.Free;
  end;
end.
