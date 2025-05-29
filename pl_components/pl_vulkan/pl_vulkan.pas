{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit pl_vulkan;

{$warn 5023 off : no warning about unused units}
interface

uses
  vulkanlib, vulkanobjects, vulkanutils, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pl_vulkan', @Register);
end.
