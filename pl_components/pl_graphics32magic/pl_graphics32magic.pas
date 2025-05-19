{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit pl_Graphics32Magic;

{$warn 5023 off : no warning about unused units}
interface

uses
  AllGraphic32MagicReg, gmChannelIO, gmChannelManager, gmChannels, 
  gmChannelViewer, gmRGBChannelManager, gmChannelCommands, gmCommandViewer, 
  gmComplexCommands, gmHistoryCommands, gmLayerCommands, gmMiscCommandIcons, 
  gmPathCommands, gmRichTextLayerCommands, gmSelectionCommands, 
  gmShapeRegionLayerCommands, gmVectorLayerCommands, gmAddNoise, gmAlphaFuncs, 
  gmAntialias, gmApplyImage, gmArtFragmentationFilter, gmBlendModes, 
  gmBoxBlurFilter, gmBrushes, gmChannelMixer, gmColorBalance, gmColorRange, 
  gmColorSpace, gmColorTransfer, gmColorTransform, gmCommonDataModule, 
  gmCommonFuncs, gmConstants, gmConvolve, gmConvolveFilter, gmCrop, 
  gmCurvesTool, gmDiagonal, gmExpBlur, gmFigures, gmFill, gmFlareFX, 
  gmGammaTuner, gmGaussianBlurFilter, gmGimpBaseCurves, gmGimpBaseEnums, 
  gmGimpColorBar, gmGimpColorSpace, gmGimpColorTypes, gmGimpCommonFuncs, 
  gmGimpDrawTool, gmGimpEmboss, gmGimpGaussianBlur, gmGimpGlassTile, 
  gmGimpHistogram, gmGimpLut, gmGimpPaintFuncs, gmGimpRGB, gmGradientMap, 
  gmGtkEnums, gmGTypes, gmGUIFuncs, gmHighPass, gmHistogramEqualize, 
  gmHueCircling, gmIllusion, gmImageMath, gmImageProcessFuncs, gmImageTile, 
  gmIO, gmJHSmartBlurFilter, gmKernel, gmLevels, gmLevelsTool, 
  gmLinearAutoContrast, gmLinearAutoLevel, gmMagneticLasso, gmMath, gmMeasure, 
  gmMosaic, gmMotionBlur, gmMotionBlurFilter, gmOilPaintingFilter, 
  gmPaintBucket, gmPaintFuncs, gmPatterns, gmPencilSketch, gmPenTools, 
  gmPluginFuncs, gmPluginManager, gmPrintOptions, gmRandomRipple, gmRegions, 
  gmRemoveRedEyes, gmReplaceColor, gmResamplers, gmResynth, gmReversalFilm, 
  gmSelection, gmSepia, gmShapes, gmSharpen, gmSmartBlurFilter, gmSolarize, 
  gmSplitBlur, gmSplitColor, gmSplitImage, gmSqueeze, gmSwatches, gmTwist, 
  gmTypes, gmUnsharpMask, gmWave, gmBrightContrastLayer, gmChannelMixerLayer, 
  gmColorBalanceLayer, gmCurvesLayer, gmGradientMapLayer, 
  gmHueSaturationLayer, gmInvertLayer, gmLayerFigureManager, gmLayerIO, 
  gmLayerPanelManager, gmLayers, gmLevelsLayer, gmPatternLayer, 
  gmPosterizeLayer, gmRichTextLayer, gmShapeRegionLayer, gmSolidColorLayer, 
  gmThresholdLayer, gmVectorLayer, gmPathIO, gmPathManager, gmPaths, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('AllGraphic32MagicReg', @AllGraphic32MagicReg.Register);
end;

initialization
  RegisterPackage('pl_Graphics32Magic', @Register);
end.
