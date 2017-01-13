program VisualJames;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  DelphiSettings in 'DelphiSettings.pas',
  Converters in '..\utils\Converters.pas',
  DelphiVersionInfo in 'DelphiVersionInfo.pas',
  Writers in '..\utils\Writers.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
