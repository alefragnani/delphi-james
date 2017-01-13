program james;

{$APPTYPE CONSOLE}

{$R *.res}

/// Supported Parameters
///  -l = Load Delphi Settings
///  -a = Apply Delphi Settings

uses
  System.SysUtils,
  DelphiSettings;

var
  FDelphiSettings: TDelphiSettings;
  param1, jamesFile: string;
begin
  if FindCmdLineSwitch('l', param1, true, [clstValueAppended]) then
  begin
    if param1 = '' then
    begin
      WriteLn('Error: No version defined. Use -l:VERSION');
      exit;
    end;
    jamesFile := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + '.james';
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
  end
  else
  begin
    // no params, look for '.james' file to 'apply'
    if ParamCount = 0 then
    begin
      param1 := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + '.james';
      if not FileExists(param1) then
      begin
        WriteLn('Error: File ".james" does not exists.');
        Exit;
      end;
    end
    else
    begin
      //param1 := ParamStr(1);
      if not FindCmdLineSwitch('a', param1, true, [clstValueAppended]) then
      begin
        WriteLn('Error: Invalid parameter.');
        Writeln('       Supported values are: ');
        Writeln('         -l:<Delphi Version>');
        Writeln('         -a:<Path to .james file>');
        Exit;
      end;

      if not FileExists(param1) then
      begin
        WriteLn('Error: File "' + param1 + '" does not exists.');
        Exit;
      end;
    end;

    FDelphiSettings := TDelphiSettings.Create;
    try
      FDelphiSettings.LoadDelphiSettingsFromJSON(param1);
      FDelphiSettings.SaveDelphiSettings;
      WriteLn('Success: Settings applied to ' + FDelphiSettings.Version);
    finally
      FDelphiSettings.Free;
    end;
  end;
end.
