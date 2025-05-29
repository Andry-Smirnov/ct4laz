{**********************************************************************
                PilotLogic Software House
  
 Package pl_Cindy
 This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)
***********************************************************************}

unit AllCindyRegister;

{$mode objfpc}{$H+}

interface
uses
  Classes, TypInfo,
  lresources, lclintf,PropEdits, ComponentEditors,

  cyBevel,
  cyTabControl, cyPageControl,
  cyCustomImage, cyScrollBox,
  cyPanel, cyAdvPanel, cyNavPanel,
  cyPaintBox, cyAdvPaintBox, cyLabel,
  cyHotLabel, cySpeedButton, cyAdvSpeedButton, cyBitBtn,
  cySkinButton, cySkinArea, cyLed, cyAdvLed, cySimpleGauge, cySplitter,
  cyAttract, cyColorMatrix, cyColorGrid, cyPieGauge, cyStatusBar, cyBook,
  cyVirtualGrid, cyVirtualChart,
  cyIniForm,
  cySearchFiles, cyFlyingContainer, cyModalContainer,
  cyResizer,    cyProgressionPanel, cyAdvProgressionPanel,
  cyRunTimeResize,
  cyMathParser, cyDebug,
  cyBookmarks, cyFieldLink,
  cyFlowPanel, cyAdvFlowPanel,
  cyGridPanel, cyAdvGridPanel,
  cyEdit, cyEditDate, cyEditTime, cyEditInteger, cyEditFloat,
  cyAdvButton,
  cyBaseCombobox,
  cyBaseFilterComboBox,
  cyButton,
  cyButtonedEdit,
  cyCheckbox,
  cyRadioGroup,
  cyMaskEdit,
  cyEditFilename, cyEditWebsite, cyEditMail,
  cyStaticText,
  cyAdvStaticText,

  //--- DB ------

  cyDBLabel,
  cyDBHotLabel,
  cyDBLed,
  cyDBAdvLed,
  cyDBSimpleGauge,
  cyDBEdit,
  cyDBMemo,
  cyDBAdvSpeedButton,
  cyDBRadioButton,
  cyDBSpeedButton;

const
  RSVisualCompsPalette        = 'Cindy Base';  
  RSVisualAdvCompsPalette     = 'Cindy Base Adv';
  RSVisualEditCompsPalette    = 'Cindy Base Edit';
  RSDataVisualCompsPalette    = 'Cindy Data Visual';
  RSDataNonVisualCompsPalette = 'Cindy Data Non-Visual';
  RSNonVisualCompsPalette     = 'Cindy Non-Visual';
  RSIEWrappersPalette         = 'Cindy IE Wrappers';




procedure Register;

implementation

{$R AllCindyRegister.res}

procedure Register;
begin
//==================================================
   RegisterComponents(RSVisualCompsPalette, [
    TcyBevel,
    TcyPanel,
    TcyNavPanel,
    TcyPaintBox,  
    TcyImage,
    TcyLabel,
    TcyHotLabel,
    TcyButton,
    TcyButtonedEdit,
    TcySpeedButton,
    TcyBitBtn,
    TcySkinButton,
    tcySkinArea,

    TcyStaticText,

    TcyCombobox,
    TcyFilterCombobox,
    TcyCheckBox,
    TcyRadioButton,
    TcyRadioGroup,


    TcyLed,
    TcySimpleGauge,
    TcyPieGauge,
    TcySplitter,

    TcyFlowPanel,

    TcyGridPanel,

    TcyAttract,
    TcyColorMatrix,
    TcyColorGrid,
    TcyStatusBar,
    TcyScrollBox,
    TcyPageControl,
    TcyTabControl,
    TcyBook
    ]);    
//==================================================
   RegisterComponents(RSVisualEditCompsPalette, [

    TcyEdit,
    TcyEditInteger,
    TcyEditFloat,
    TcyEditDate,
    TcyEditTime,
    TcyEditFilename,
    TcyEditWebsite,
    TcyEditMail,
    TcyMaskEdit
    ]);

//==================================================
   RegisterComponents(RSVisualAdvCompsPalette, [
    TcyAdvPanel,
    TcyAdvPaintBox,
    TcyAdvSpeedButton,
    TcyAdvButton,
    TcyAdvStaticText,
    TcyAdvLed,
    TcyAdvFlowPanel,
    TcyAdvGridPanel
    ]);

   
//==================================================
    RegisterComponents(RSNonVisualCompsPalette, [
    TcySearchFiles,
    TcyFlyingContainer,
    TcyModalContainer,
    TcyResizer,
    TcyVirtualGrid,
    TcyVirtualChart,
    TcyProgressionPanel,
    TcyAdvProgressionPanel,
    TcyRunTimeResize,
    TcyMathParser,
    TcyIniForm,
    TcyDebug
    ]);
   

//==================================================
    RegisterComponents(RSDataVisualCompsPalette, [
    TcyDBLabel,
    TcyDBHotLabel,
    TcyDBEdit,
    TcyDBRadioButton,
    TcyDBSpeedButton,  
    TcyDBAdvSpeedButton,
    TcyDBLed,
    TcyDBAdvLed,
    TcyDBSimpleGauge,
    TcyDBMemo
    ]);
     
//==================================================
    RegisterComponents(RSDataNonVisualCompsPalette, [
    TcyBookmarks,
    TcyFieldLink
    ]);
     
end;


end.

