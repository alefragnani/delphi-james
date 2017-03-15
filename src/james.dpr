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
    WriteLn('** Delphi James v.0.3.0.0 **');
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
      //WriteLn('Error: No version defined. Use -l:VERSION');
      WriteParameterError('Error: No version defined. Use -l:VERSION');
      exit;
    end;

    jamesFile := IncludeTrailingPathDelimiter(GetCurrentDir) + '.james';
    FDelphiSettings := TDelphiSettings.Create;
    try
      try
        FDelphiSettings.Version := 'Delphi ' + param1;
        FDelphiSettings.LoadDelphiSettings;
        FDelphiSettings.SaveDelphiSettingsToJSON(jamesFile);
        WriteLn('Success: Settings saved to .james file');
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
      //WriteLn('Error: File ".james" does not exists.');
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
//      WriteLn('Error: Invalid parameter.');
//      Writeln('       Supported values are: ');
//      Writeln('         -l:<Delphi Version>');
//      Writeln('         -a:<Path to .james file>');
      Exit;
    end;

    param1 := ExpandFileName(param1);
    if not FileExists(param1) then
    begin
      //WriteLn('Error: File "' + param1 + '" does not exists.');
      WriteParameterError('Error: File "' + param1 + '" does not exists.');
      Exit;
    end;
  end;

  FDelphiSettings := TDelphiSettings.Create;
  try
    if FDelphiSettings.LoadDelphiSettingsFromJSON(param1) then
    begin
      FDelphiSettings.SaveDelphiSettings;
      WriteLn('Success: Settings applied to ' + FDelphiSettings.Version);
    end
    else
      WriteLn('Error: ' + FDelphiSettings.LastError);
  finally
    FDelphiSettings.Free;
  end;
end.
