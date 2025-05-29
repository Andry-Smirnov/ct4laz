{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit pl_cindy;

{$warn 5023 off : no warning about unused units}
interface

uses
  AllCindyRegister, cyAdvButton, cyAdvFlowPanel, cyAdvGridPanel, cyAdvLed, 
  cyAdvPaintBox, cyAdvPanel, cyAdvProgressionPanel, cyAdvSpeedButton, 
  cyAdvStaticText, cyAttract, cyBaseButton, cyBaseColorMatrix, cyBaseCombobox, 
  cyBaseContainer, cyBaseExtCtrls, cyBaseFilterComboBox, cyBaseFlowPanel, 
  cyBaseGridPanel, cyBaseLabel, cyBaseLed, cyBaseMeasure, cyBasePanel, 
  cyBaseSpeedButton, cyBaseStaticText, cyBevel, cyBitBtn, cyBook, 
  cyBookmarkList, cyBookmarks, cyButton, cyButtonedEdit, cyCheckbox, 
  cyClasses, cyColorGrid, cyColorMatrix, cyCustomGauge, cyCustomImage, 
  cyCustomMeasure, cyCustomProgressionPanel, cyDateUtils, cyDBAdvLed, 
  cyDBEdit, cyDBHotLabel, cyDBLabel, cyDBLed, cyDBSimpleGauge, cyDebug, 
  cyDmmCanvas, cyEdit, cyEditDate, cyEditFilename, cyEditFloat, cyEditInteger, 
  cyEditMail, cyEditTime, cyEditWebsite, cyFieldLink, cyFlowPanel, 
  cyFlyingContainer, cyGraphics, cyGridPanel, cyHotLabel, cyImage, cyIniForm, 
  cyLabel, cyLed, cyMaskEdit, cyMathParser, cyModalContainer, cyNavPanel, 
  cyObjUtils, cyPageControl, cyPaintBox, cyPanel, cyPieGauge, 
  cyProgressionPanel, cyRadioGroup, cyResizer, cyRunTimeResize, cyScrollBox, 
  cySearchFiles, cySimpleGauge, cySkinArea, cySkinButton, cySpeedButton, 
  cySplitter, cyStaticText, cyStatusBar, cyStrUtils, cyTabControl, cyTypes, 
  cyVirtualChart, cyVirtualGrid, cyDBAdvSpeedButton, cyDBRadioButton, 
  cyDBMemo, cyDBSpeedButton, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('AllCindyRegister', @AllCindyRegister.Register);
end;

initialization
  RegisterPackage('pl_cindy', @Register);
end.
