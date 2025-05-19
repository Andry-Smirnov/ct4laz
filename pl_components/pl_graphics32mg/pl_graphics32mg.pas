{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit pl_graphics32mg;

{$warn 5023 off : no warning about unused units}
interface

uses
  alligregister, bivTheme_Standard, igBase, igbestream, igBmp, 
  igBrightContrastLayer, igComboboxBlendModes, igCore_Items, igCore_rw, 
  igCore_Viewer, igGradient, igGradient_rwPhotoshopGRD, igGrid, 
  igGrid_ListView, igGrid_ListView_Layers, igJpg, igLayerIO, 
  igLayerPanelManager, igLayers, igLayersListBox, igMath, igPaintFuncs, 
  igPattern, igSwatch, igSwatch_ListView, igSwatch_rwACO, igSwatch_rwASE, 
  igSwatch_rwGPL, igTool_BrushSimple, igTool_CustomBrush, igTool_PaintBrush, 
  igTool_PencilSimple, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('alligregister', @alligregister.Register);
end;

initialization
  RegisterPackage('pl_graphics32mg', @Register);
end.
