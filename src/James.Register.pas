unit James.Register;

interface

uses
  Classes,
  System.SysUtils,
  System.Win.Registry,
  Winapi.Messages,
  Winapi.Windows;

type
  TRegister = class
  private
    class procedure BroadcastChange;
  public
    class procedure RegisterInPath(const psFolder: string);
  end;

implementation

// http://edn.embarcadero.com/article/28254
class procedure TRegister.BroadcastChange;
var
  lParam, wParam : Integer;   {Integers that indicate pointers to parameters}
  Buf     : Array[0..10] of Char; {Buffer used to indicate what setting we have changed.}
  aResult : Cardinal;         {Error Number returned from API Call}
begin
  {Now comes the interesting part.}
  {Environment is the section of global settings we want the system to update}
   Buf := 'Environment';
   wParam := 0;
   {This gives us a pointer to the Buffer for Windows to read.}
   lParam := Integer(@Buf[0]);

   {Here we make a call to SendMessageTimeout to Broadcast a message to the
   entire system telling every application (including explorer) to update
   its settings}
   SendMessageTimeout(HWND_BROADCAST ,
                      WM_SETTINGCHANGE ,
                      wParam,
                      lParam,
                      SMTO_NORMAL	,
                      4000,
                      aResult);

   {Display windows lasterror if the result is an error.}
   if aResult <> 0 then
   begin
     SysErrorMessage(aResult);
   end;
end;

class procedure TRegister.RegisterInPath(const psFolder: string);
var
  registry: TRegistry;
  currentKeyValue: string;
  values: TStringList;
begin
  registry := TRegistry.Create;
  try
    if registry.OpenKey('Environment', false) then
    begin
      currentKeyValue := registry.ReadString('Path');
      if (currentKeyValue <> '') then
      begin
        values := TStringList.Create;
        try
          values.Delimiter := ';';
          values.DelimitedText := currentKeyValue;
          if values.IndexOf(psFolder) = -1 then
          begin
            values.Add(psFolder);
            registry.WriteString('Path', values.DelimitedText);

            BroadcastChange;
          end;
        finally
          values.free;
        end;
      end;
    end;
  finally
    registry.CloseKey;
  end;
end;

end.
