{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit min_kzdesktop;

interface

uses
  KZLazDesktop, kzdesktop, uKZConfig, KZDeskManagers, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('kzdesktop', @kzdesktop.Register);
end;

initialization
  RegisterPackage('min_kzdesktop', @Register);
end.
