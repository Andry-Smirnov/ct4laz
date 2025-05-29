{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit pl_USB;

{$warn 5023 off : no warning about unused units}
interface

uses
  EZUSB, IntelHex, LibUsbDyn, LibUsbOop, LibUsbUtil, PasLibUsbUtils, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pl_USB', @Register);
end.
