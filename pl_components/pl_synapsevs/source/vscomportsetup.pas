
{**********************************************************************
 Package pl_synapseVS
 From PilotLogic Software House(https://www.pilotlogic.com/)
 This file is part of CodeTyphon Studio
***********************************************************************}

unit vsComPortsetup;

{$mode objfpc}{$H+}



interface

uses
  LCLIntf, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, LResources,
  synaser, vsComPortbase;

type

  TComSetupFrm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ComComboBox1: TComboBox;
    ComComboBox2: TComboBox;
    ComComboBox3: TComboBox;
    ComComboBox4: TComboBox;
    ComComboBox5: TComboBox;
    ComComboBox6: TComboBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;


implementation

{$R *.lfm}


procedure TComSetupFrm.FormCreate(Sender: TObject);
begin
  ComComboBox1.Items.CommaText :=  GetSerialPortNames();
  StringArrayToList(ComComboBox2.Items,BaudRateStrings) ;
  StringArrayToList(ComComboBox3.Items,DataBitsStrings) ;
  StringArrayToList(ComComboBox4.Items,StopBitsStrings) ;
  StringArrayToList(ComComboBox5.Items,ParityBitsStrings) ;
  StringArrayToList(ComComboBox6.Items,FlowControlStrings) ;
end;

end.
