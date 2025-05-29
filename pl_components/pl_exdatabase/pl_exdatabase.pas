{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit pl_exdatabase;

{$warn 5023 off : no warning about unused units}
interface

uses
  AllExDatabaseReg, TplDBTreeviewUnit, TplDBTreeviewBaseUnit, 
  TpBufDatasetTempUnit, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('AllExDatabaseReg', @AllExDatabaseReg.Register);
end;

initialization
  RegisterPackage('pl_exdatabase', @Register);
end.
