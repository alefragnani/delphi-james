unit DelphiVersionInfo;

interface

type
  TDelphiVersionInfo = class
    Description: string;
    RegistryKey: string;
    RegistryKeyLibraryPath: string;
    RegistryKeyEnvironmentVariables: string;
    constructor Create(const pDescription, pRegistryKey, pLibraryPath, pEnvironmentVariables: string);
  end;

implementation

constructor TDelphiVersionInfo.Create(const pDescription, pRegistryKey, pLibraryPath, pEnvironmentVariables: string);
begin
  self.Description := pDescription;
  self.RegistryKey := pRegistryKey;
  self.RegistryKeyLibraryPath := pLibraryPath;
  self.RegistryKeyEnvironmentVariables := pEnvironmentVariables;
end;


end.
