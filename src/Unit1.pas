unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, SynEditHighlighter,
  SynHighlighterJSON, SynEdit, DelphiSettings, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    lbKnownPackages: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    lbLibraryPath: TListBox;
    Label3: TLabel;
    cbDelphiVersion: TComboBox;
    btLoadFromRegistry: TButton;
    btSaveToRegistry: TButton;
    SynEdit1: TSynEdit;
    SynJSONSyn1: TSynJSONSyn;
    Label4: TLabel;
    lbEnvironmentVariables: TListBox;
    btLoadFromFile: TButton;
    btSaveToFile: TButton;
    procedure btLoadFromRegistryClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbDelphiVersionSelect(Sender: TObject);
    procedure btSaveToRegistryClick(Sender: TObject);
    procedure btSaveToFileClick(Sender: TObject);
    procedure btLoadFromFileClick(Sender: TObject);
  private
    { Private declarations }
    FDelphiSettings: TDelphiSettings;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.btSaveToFileClick(Sender: TObject);
begin
  FDelphiSettings.SaveDelphiSettingsToJSON(IncludeTrailingPathDelimiter(
    ExtractFilePath(Application.ExeName)) + '.james');
end;

procedure TForm1.btSaveToRegistryClick(Sender: TObject);
begin
  FDelphiSettings.SaveDelphiSettings;
end;

procedure TForm1.btLoadFromFileClick(Sender: TObject);
begin
  SynEdit1.Lines.Clear;
  SynEdit1.Lines.LoadFromFile(IncludeTrailingPathDelimiter(
    ExtractFilePath(Application.ExeName)) + '.james');
  // clear
  lbKnownPackages.Items.Clear;
  lbLibraryPath.Items.Clear;
  lbEnvironmentVariables.Items.Clear;

  FDelphiSettings.LoadDelphiSettingsFromJSON(IncludeTrailingPathDelimiter(
    ExtractFilePath(Application.ExeName)) + '.james');
  lbKnownPackages.Items.AddStrings(FDelphiSettings.KnownPackages);
  lbLibraryPath.Items.AddStrings(FDelphiSettings.LibraryPath);
  lbEnvironmentVariables.Items.AddStrings(FDelphiSettings.EnvironmentVariables);
end;

procedure TForm1.btLoadFromRegistryClick(Sender: TObject);
begin
  // clear
  lbKnownPackages.Items.Clear;
  lbLibraryPath.Items.Clear;
  lbEnvironmentVariables.Items.Clear;

  // load
  FDelphiSettings.LoadDelphiSettings;
  lbKnownPackages.Items.AddStrings(FDelphiSettings.KnownPackages);
  lbLibraryPath.Items.AddStrings(FDelphiSettings.LibraryPath);
  lbEnvironmentVariables.Items.AddStrings(FDelphiSettings.EnvironmentVariables);
end;

procedure TForm1.cbDelphiVersionSelect(Sender: TObject);
begin
  FDelphiSettings.Version := cbDelphiVersion.Text;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FDelphiSettings := TDelphiSettings.Create;
  cbDelphiVersion.Items.AddStrings(FDelphiSettings.InstalledVersions);
end;

end.
