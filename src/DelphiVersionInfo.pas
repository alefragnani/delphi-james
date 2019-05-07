//-------------------------------------------------------------------------------------------------
//
//    James - The Delphi Project Manager
//
//    Copyright (C) 2017  Alessandro Fragnani
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, version 3 of the License.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <https://www.gnu.org/licenses/>.
//
//    https://github.com/alefragnani/delphi-james
//
//-------------------------------------------------------------------------------------------------

unit DelphiVersionInfo;

interface

uses
  System.SysUtils;

const
  TAG_PLATFORM = '%PLATFORM%';

type
  TDelphiPlatform = (dpWin32, dpWin64, dpiOSDevice32, dpiOSDevice64, dpiOSSimulator,  dpOSX32, dpAndroid32);
  TDelphiPlatforms = set of TDelphiPlatform;

  TDelphiVersion = class
  protected
    function GetSupportedPlatforms: TDelphiPlatforms; virtual;
  public
    Description: string;
    RegistryKey: string;
    RegistryKeyKnownPackages: string;
    RegistryKeyLibraryPath: string;
    RegistryKeyEnvironmentVariables: string;

    constructor Create; virtual; abstract;

    class function PlatformStr(eplatform: TDelphiPlatform): string;
    function GetLibraryPathForPlatform(eplatform: TDelphiPlatform): string; virtual;
    function SupportsVariousPlatforms: boolean;
    property SupportedPlatforms: TDelphiPlatforms read GetSupportedPlatforms;
  end;

  TDelphi5 = class(TDelphiVersion)
  public
    constructor Create; override;
    function GetLibraryPathForPlatform(eplatform: TDelphiPlatform): string; override;
  end;

  TDelphi2006 = class(TDelphiVersion)
  public
    constructor Create; override;
  end;

  TDelphiSeattle = class(TDelphiVersion)
  protected
    function GetSupportedPlatforms: TDelphiPlatforms; override;
  public
    constructor Create; override;
  end;

  TDelphiBerlin = class(TDelphiVersion)
  protected
    function GetSupportedPlatforms: TDelphiPlatforms; override;
  public
    constructor Create; override;
  end;

  TDelphiTokyo = class(TDelphiVersion)
  protected
    function GetSupportedPlatforms: TDelphiPlatforms; override;
  public
    constructor Create; override;
  end;

  TDelphiRio = class(TDelphiVersion)
  protected
    function GetSupportedPlatforms: TDelphiPlatforms; override;
  public
    constructor Create; override;
  end;

implementation

function TDelphiVersion.GetSupportedPlatforms: TDelphiPlatforms;
begin
  result := [dpWin32];
end;

class function TDelphiVersion.PlatformStr(eplatform: TDelphiPlatform): string;
begin
  case eplatform of
    dpWin32: result := 'Win32';
    dpWin64: result := 'Win64';
    dpiOSDevice32: result := 'iOSDevice32';
    dpiOSDevice64: result := 'iOSDevice64';
    dpiOSSimulator: result := 'iOSSimulator';
    dpOSX32: result := 'OSX32';
    dpAndroid32: result := 'Android32';
  end;
end;

function TDelphiVersion.GetLibraryPathForPlatform(eplatform: TDelphiPlatform): string;
begin
  result := StringReplace(Self.RegistryKeyLibraryPath, TAG_PLATFORM, PlatformStr(eplatform), [rfIgnoreCase]);
end;

function TDelphiVersion.SupportsVariousPlatforms: boolean;
var
  supported: TDelphiPlatforms;
  count, i: byte;
begin
  supported := Self.GetSupportedPlatforms;
  count := 0;
  for i := Ord(Low(TDelphiPlatform)) to Ord(High(TDelphiPlatform)) do
  begin
    if TDelphiPlatform(i) in Self.SupportedPlatforms then
      inc(count);
  end;
  result := count > 1;
end;

constructor TDelphi5.Create;
begin
  Self.Description := 'Delphi 5';
  Self.RegistryKey := 'Software\Borland\Delphi\5.0';
  Self.RegistryKeyKnownPackages := 'Software\Borland\Delphi\5.0\Known Packages';
  Self.RegistryKeyLibraryPath := 'Software\Borland\Delphi\5.0\Library';
  Self.RegistryKeyEnvironmentVariables := '';
end;

function TDelphi5.GetLibraryPathForPlatform(eplatform: TDelphiPlatform): string;
begin
  if eplatform = dpWin32 then
    result := Self.RegistryKeyLibraryPath
  else
    result := '';
end;

constructor TDelphi2006.Create;
begin
  Self.Description := 'Delphi 2006';
  Self.RegistryKey := 'Software\Borland\BDS\4.0';
  Self.RegistryKeyKnownPackages := 'Software\Borland\BDS\4.0\Known Packages';
  Self.RegistryKeyLibraryPath := 'Software\Borland\BDS\4.0\Library';
  Self.RegistryKeyEnvironmentVariables := '';
end;


constructor TDelphiSeattle.Create;
begin
  Self.Description := 'Delphi Seattle';
  Self.RegistryKey := 'Software\Embarcadero\BDS\17.0';
  Self.RegistryKeyKnownPackages := 'Software\Embarcadero\BDS\17.0\Known Packages';
  Self.RegistryKeyLibraryPath := 'Software\Embarcadero\BDS\17.0\Library\' + TAG_PLATFORM;
  Self.RegistryKeyEnvironmentVariables := 'Software\Embarcadero\BDS\17.0\Environment Variables';
end;

function TDelphiSeattle.GetSupportedPlatforms: TDelphiPlatforms;
begin
  result := [dpWin32, dpWin64, dpiOSDevice32, dpiOSDevice64, dpiOSSimulator,  dpOSX32, dpAndroid32];
end;

constructor TDelphiBerlin.Create;
begin
  Self.Description := 'Delphi Berlin';
  Self.RegistryKey := 'Software\Embarcadero\BDS\18.0';
  Self.RegistryKeyKnownPackages := 'Software\Embarcadero\BDS\18.0\Known Packages';
  Self.RegistryKeyLibraryPath := 'Software\Embarcadero\BDS\18.0\Library\' + TAG_PLATFORM;
  Self.RegistryKeyEnvironmentVariables := 'Software\Embarcadero\BDS\18.0\Environment Variables';
end;

function TDelphiBerlin.GetSupportedPlatforms: TDelphiPlatforms;
begin
  result := [dpWin32, dpWin64, dpiOSDevice32, dpiOSDevice64, dpiOSSimulator,  dpOSX32, dpAndroid32];
end;

constructor TDelphiTokyo.Create;
begin
  Self.Description := 'Delphi Tokyo';
  Self.RegistryKey := 'Software\Embarcadero\BDS\19.0';
  Self.RegistryKeyKnownPackages := 'Software\Embarcadero\BDS\19.0\Known Packages';
  Self.RegistryKeyLibraryPath := 'Software\Embarcadero\BDS\19.0\Library\' + TAG_PLATFORM;
  Self.RegistryKeyEnvironmentVariables := 'Software\Embarcadero\BDS\19.0\Environment Variables';
end;

function TDelphiTokyo.GetSupportedPlatforms: TDelphiPlatforms;
begin
  result := [dpWin32, dpWin64, dpiOSDevice32, dpiOSDevice64, dpiOSSimulator,  dpOSX32, dpAndroid32];
end;

{ TDelphiRio }

constructor TDelphiRio.Create;
begin
  Self.Description := 'Delphi Rio';
  Self.RegistryKey := 'Software\Embarcadero\BDS\20.0';
  Self.RegistryKeyKnownPackages := 'Software\Embarcadero\BDS\20.0\Known Packages';
  Self.RegistryKeyLibraryPath := 'Software\Embarcadero\BDS\20.0\Library\' + TAG_PLATFORM;
  Self.RegistryKeyEnvironmentVariables := 'Software\Embarcadero\BDS\20.0\Environment Variables';
end;

function TDelphiRio.GetSupportedPlatforms: TDelphiPlatforms;
begin
  result := [dpWin32, dpWin64, dpiOSDevice32, dpiOSDevice64, dpiOSSimulator,  dpOSX32, dpAndroid32];
end;

end.
