unit JamesExpertProjectManagerContextMenu;

interface

uses
  Vcl.Menus,
  ToolsAPI;

type
  TJameExpertProjectManagerContextMenu = class(TNotifierObject, INTAProjectMenuCreatorNotifier)
  private
    procedure MyContextMenuClickHandler(Sender: TObject);
  public
    function AddMenu(const Ident: string): TMenuItem;
    function CanHandle(const Ident: string): Boolean;
  end;

procedure Register;

implementation

uses
  Vcl.Dialogs,
  SysUtils,
  ShellAPI,
  Vcl.Forms,
  Windows,
  DelphiSettings;

const
  DELPHI_VERSION =
    {$IFDEF VER180} '2006' {$ENDIF} // Turbo Delphi
    {$IFDEF VER310} 'Berlin' {$ENDIF} // Berlin
    {$IFDEF VER320} 'Tokyo' {$ENDIF} // Tokyo
    ;

function TJameExpertProjectManagerContextMenu.AddMenu(const Ident: string): TMenuItem;
begin
  result := TMenuItem.Create(nil);
  result.Caption := 'James - Load Delphi Settings';
  result.OnClick := MyContextMenuClickHandler;
end;

function TJameExpertProjectManagerContextMenu.CanHandle(const Ident: string): Boolean;
begin
  result := Ident = sProjectContainer;
end;

procedure TJameExpertProjectManagerContextMenu.MyContextMenuClickHandler(Sender: TObject);
var
  lIdent, jamesFilePath: string;
  oDelphiSettings: TDelphiSettings;
begin
  lIdent := '';
  (BorlandIDEServices as IOTAProjectManager).GetCurrentSelection(lIdent);

  jamesFilePath := IncludeTrailingPathDelimiter(ExtractFileDir(lIdent)) + '.james';
  oDelphiSettings := TDelphiSettings.Create;
  try
    try
      oDelphiSettings.Version := 'Delphi '+ DELPHI_VERSION;
      oDelphiSettings.LoadDelphiSettings;
      oDelphiSettings.SaveDelphiSettingsToJSON(jamesFilePath);
      ShowMessage('Settings saved to ' + jamesFilePath);
    except
      on e: EDelphiVersionNotSupported do
        ShowMessage('Error: ' + e.Message);
    end;
  finally
    oDelphiSettings.Free;
  end;
end;

var
  FNotifierIndex: Integer;

procedure Register;
begin
  FNotifierIndex := (BorlandIDEServices as IOTAProjectManager).AddMenuCreatorNotifier(TJameExpertProjectManagerContextMenu.Create);
end;

initialization
  FNotifierIndex := -1;

finalization
  if FNotifierIndex > -1 then
    (BorlandIDEServices as IOTAProjectManager).RemoveMenuCreatorNotifier(FNotifierIndex);


end.
