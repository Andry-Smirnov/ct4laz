{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit pl_graphics32vpr;

{$warn 5023 off : no warning about unused units}
interface

uses
  AllGraphics32vprReg, VGR32_CFDG, VGR32_Clipper, VGR32_Lines, VGR32_Misc, 
  VGR32_Pictures, VGR32_PolygonsEx, VGR32_VectorGraphics, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pl_graphics32vpr', @Register);
end.
