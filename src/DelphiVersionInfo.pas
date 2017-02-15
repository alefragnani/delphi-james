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
    function GetLibraryPathForPlatform(eplatform: TDelphiPlatform): string;
    property SupportedPlatforms: TDelphiPlatforms read GetSupportedPlatforms;
  end;

  TDelphi2006 = class(TDelphiVersion)
  public
    constructor Create; override;
  end;

  TDelphiBerlin = class(TDelphiVersion)
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

constructor TDelphi2006.Create;
begin
  Self.Description := 'Delphi 2006';
  Self.RegistryKey := 'Software\Borland\BDS\4.0';
  Self.RegistryKeyKnownPackages := 'Software\Borland\BDS\4.0\Known Packages';
  Self.RegistryKeyLibraryPath := 'Software\Borland\BDS\4.0\Library';
  Self.RegistryKeyEnvironmentVariables := '';
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

end.
