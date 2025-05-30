{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pl_Graphics32;

{$warn 5023 off : no warning about unused units}
interface

uses
  AllGR32Register, GR32, GR32_AllStrings, GR32_ArrowHeads, GR32_Backends, 
  GR32_Backends_Generic, GR32_Bindings, GR32_Blend, GR32_BlendASM, 
  GR32_BlendMMX, GR32_BlendSSE2, GR32_Blurs, GR32_Brushes, GR32_Clipper, 
  GR32_ColorGradients, GR32_ColorPicker, GR32_ColorSwatch, GR32_Containers, 
  GR32_Dsgn_Bitmap, GR32_Dsgn_Color, GR32_Dsgn_ColorPicker, GR32_Dsgn_Misc, 
  GR32_ExtImage, GR32_Filters, GR32_Geometry, GR32_Image, GR32_Layers, 
  GR32_LowLevel, GR32_Math, GR32_MicroTiles, GR32_OrdinalMaps, GR32_Panel32, 
  GR32_Paths, GR32_Polygons, GR32_PolygonsAggLite, GR32_ProgressBar, 
  GR32_RangeBars, GR32_Rasterizers, GR32_RepaintOpt, GR32_Resamplers, 
  GR32_Scrollbox, GR32_System, GR32_Transforms, GR32_VectorMaps, 
  GR32_VectorUtils, GR32_VPR, GR32_VPR2, GR32_XPThemes, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('AllGR32Register', @AllGR32Register.Register);
end;

initialization
  RegisterPackage('pl_Graphics32', @Register);
end.
