unit mwin;

interface

uses

  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,  StdCtrls, ExtCtrls, Spin, ComCtrls,
  Buttons, types ,LResources,LCLIntf,TplSpiderGraphUnit;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button11: TButton;
    QSpiderGraph1: TplSpiderGraph;
    CheckBox1: TCheckBox;
    Panel1: TPanel;
    Panel2: TPanel;
    ColorDialog1: TColorDialog;
    FontDialog1: TFontDialog;
    Button1: TButton;
    GroupBox1: TGroupBox;
    Edit1: TEdit;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Label3: TLabel;
    Panel3: TPanel;
    Panel4: TPanel;
    Label4: TLabel;
    GroupBox2: TGroupBox;
    SpinEdit1: TSpinEdit;
    Label6: TLabel;
    GroupBox3: TGroupBox;
    ComboBox1: TComboBox;
    Label2: TLabel;
    Panel5: TPanel;
    Panel6: TPanel;
    Label5: TLabel;
    Label7: TLabel;
    ComboBox2: TComboBox;
    Label8: TLabel;
    Panel7: TPanel;
    Panel8: TPanel;
    Label9: TLabel;
    Panel9: TPanel;
    Panel10: TPanel;
    CheckBox2: TCheckBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    Panel11: TPanel;
    Panel12: TPanel;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    SpinEdit2: TSpinEdit;
    Label14: TLabel;
    StatusBar1: TStatusBar;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    Label17: TLabel;
    Panel15: TPanel;
    Panel16: TPanel;
    CheckBox7: TCheckBox;
    Label16: TLabel;
    Panel13: TPanel;
    Panel14: TPanel;
    Label18: TLabel;
    Label19: TLabel;
    CheckBox8: TCheckBox;
    GroupBox6: TGroupBox;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    GroupBox7: TGroupBox;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    SpinEdit3: TSpinEdit;
    Panel17: TPanel;
    Panel18: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Memo1: TMemo;
    Label21: TLabel;
    Button6: TButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    Label12: TLabel;
    RadioButton8: TRadioButton;
    Label15: TLabel;
    CheckBox13: TCheckBox;
    Button7: TButton;
    FontDialog2: TFontDialog;
    GroupBox8: TGroupBox;
    Button4: TButton;
    Button5: TButton;
    Edit3: TEdit;
    CheckBox14: TCheckBox;
    Label10: TLabel;
    Button2: TButton;
    Button3: TButton;
    Edit2: TEdit;
    FontDialog3: TFontDialog;
    Button8: TButton;
    Button10: TButton;
    Button9: TButton;
    procedure Button11Click(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Panel4Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure Panel6Click(Sender: TObject);
    procedure Panel8Click(Sender: TObject);
    procedure Panel10Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Panel12Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure Panel16Click(Sender: TObject);
    procedure Panel14Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CheckBox8Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Panel18Click(Sender: TObject);
    procedure CheckBox9Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure QSpiderGraph1MouseEnterLine(Sender: TObject;
      const lineIndex: Integer);
    procedure QSpiderGraph1MouseExitLine(Sender: TObject;
      const lineIndex: Integer);
    procedure Button6Click(Sender: TObject);
    procedure CheckBox13Click(Sender: TObject);
    procedure CheckBox14Click(Sender: TObject);
    procedure Label10Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation


procedure TForm1.FormCreate(Sender: TObject);
// We'll add three lines to our graph, as soon as the form is build :
// This illustrates the basic way of populating a graph with lines.
// The other way is shown bellow (search for "QSpiderGraph1.AddLine(anArray);" )

var anArray : TQSingleArray;                      // TQSingleArray = array of single;
                                                  // is defined in QSpiderGraph.pas ;
begin
  QSpiderGraph1.axes[0].caption := 'First axe'; // First af all, set some axes properties
  QSpiderGraph1.axes[1].caption := 'Some ratio';
  QSpiderGraph1.axes[2].caption := 'Another one';
  QSpiderGraph1.axes[3].caption := '4th one';
  QSpiderGraph1.axes[4].caption := '5th axe';
  QSpiderGraph1.axes[5].caption := 'Last one';

  QSpiderGraph1.linesCount := 3;                  // Then set the lines count to ...
                                                  // ... have the graph prepare evthg
  SetLength(anArray,6);                           // Now size a TQSingleArray, and populate it
  anArray[0] := 110.0;
  anArray[1] := 50;
  anArray[2] := 20;
  anArray[3] := 66;
  anArray[4] := 100;
  anArray[5] := 20;

  QSpiderGraph1.lines[0].values := anArray;       // Sends this line to the graph
  QSpiderGraph1.lines[0].color  := clRed;         // now setting this line particular properties
  QSpiderGraph1.lines[0].caption := 'First datas set';

  anArray[0] := 21.0;                             // and do the same again, with all our lines.
  anArray[1] := 87;
  anArray[2] := 42.53456546;
  anArray[3] := 66;
  anArray[4] := 10;
  anArray[5] := 40;
  QSpiderGraph1.lines[1].values := anArray;
  QSpiderGraph1.lines[1].color  := clGreen;
  QSpiderGraph1.lines[1].penWidth:= 2;
  QSpiderGraph1.lines[1].caption := '2nd set';

  anArray[0] := 41.0;
  anArray[1] := 37;
  anArray[2] := 92.53456546;
  anArray[3] := 46;
  anArray[4] := 60;
  anArray[5] := 80;
  QSpiderGraph1.lines[2].values := anArray;
  QSpiderGraph1.lines[2].color  := clPurple;
  QSpiderGraph1.lines[2].penWidth:= 2;
  QSpiderGraph1.lines[2].caption := 'And this is the third one...';

  // The mouse box may be too big to fit into the graph itself.
  // To prevent this, one can instruct it to use the underlying form as parent :
  QSpiderGraph1.mBoxParent := Self;

end;



// ---------------------------------------------------------------
// The axes part.
// (Filling or not the axes summits' polygon, and with what color) :
// ---------------------------------------------------------------
procedure TForm1.CheckBox1Click(Sender: TObject);
// Fill or not.
begin
  QSpiderGraph1.polygonFill := CheckBox1.Checked;
end;

procedure TForm1.Panel2Click(Sender: TObject);
// Set the polygon Color.
begin
  ColorDialog1.Color := Panel2.Color;
  If ColorDialog1.Execute Then
  Begin
    QSpiderGraph1.polygonColor := ColorDialog1.Color;
    Panel2.Color := ColorDialog1.Color;
  End;
end;

procedure TForm1.Button11Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.Panel4Click(Sender: TObject);
// Set the axes color
begin
  ColorDialog1.Color := Panel4.Color;
  If ColorDialog1.Execute Then
  Begin
    QSpiderGraph1.axesColor := ColorDialog1.Color;
    Panel4.Color := ColorDialog1.Color;
  End;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
// Surround axes captions with a frame
begin
  QSpiderGraph1.axesCaptionsFramed := CheckBox2.Checked;
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
// Changing axesCount (resetts lines too...)
begin
  QSpiderGraph1.axesCount := SpinEdit1.Value;
end;


procedure TForm1.Button7Click(Sender: TObject);
// Sets The axes captions font.
begin
  Try FontDialog2.Font := QSpiderGraph1.axesFont ; Except; End;
  If FontDialog2.Execute Then
  Begin
    QSpiderGraph1.axesFont := FontDialog2.Font;
    QSpiderGraph1.Invalidate;
  End;
end;


// ---------------------------------------------------------------
// Dealing with the title.
// ---------------------------------------------------------------
procedure TForm1.Button1Click(Sender: TObject);
// Changing the graph's title font
begin
  Try FontDialog1.Font := QSpiderGraph1.titleFont ; Except; End;
  If FontDialog1.Execute Then
  Begin
    Button1.Caption := FontDialog1.Font.Name;
    QSpiderGraph1.titleFont := FontDialog1.Font;
    QSpiderGraph1.Invalidate;
  End;
end;

procedure TForm1.Edit1Change(Sender: TObject);
// Changing the graph's title
begin
  If Edit1.Modified Then QSpiderGraph1.titleCaption := Edit1.Text;
end;

procedure TForm1.RadioButton1Click(Sender: TObject);
// Setting the title position (top or bottom)
begin
  If RadioButton1.Checked
     Then QSpiderGraph1.titlePosition := qtpTop
     Else QSpiderGraph1.titlePosition := qtpBottom;
end;

// ---------------------------------------------------------------
// The graph general layout
// ---------------------------------------------------------------
procedure TForm1.ComboBox1Change(Sender: TObject);
// Border style
begin
  Case ComboBox1.ItemIndex Of
    0 :  QSpiderGraph1.borderStyle := bsNone ;
    1 :  QSpiderGraph1.borderStyle := bsFlat ;
    Else QSpiderGraph1.borderStyle := bs3D ;
  End;
end;


procedure TForm1.ComboBox2Change(Sender: TObject);
// Background layout : transparent, single color or gradient
begin
  // For the purpose of the demo, QSpiderGraph and the demo Form settings
  // are managed in two different loops..

  // -1- Dealing with the graph's instance
  Case ComboBox2.ItemIndex Of
    0 :  QSpiderGraph1.backGround := bgTransparent ;
    1 :  QSpiderGraph1.backGround := bgColored ;
    2 :  QSpiderGraph1.backGround := bgTopBottom ;
    3 :  QSpiderGraph1.backGround := bgBottomTop ;
    4 :  QSpiderGraph1.backGround := bgLeftToRight ;
    Else QSpiderGraph1.backGround := bgRightToLeft ;
  End;

  // -2- Now, the form's settings, whose role is to illustrate which
  //     property goes with which other.
  Case ComboBox2.ItemIndex Of
    0 :  {bgTransparent Disable buttons}
         Begin
           Label5.Enabled := False;
           Panel6.Enabled := False;
           Panel6.BevelOuter := bvNone;
           LAbel8.Enabled := False;
           Panel8.Enabled := False;
           Panel8.BevelOuter := bvNone;
           Label9.Enabled := False;
           Panel10.Enabled:= False;
           Panel10.BevelOuter := bvNone;
         End;
    1 :  {bgColored}
         Begin
           Label5.Enabled := True;
           Panel6.Enabled := True;
           Panel6.BevelOuter := bvLowered;
           LAbel8.Enabled := False;
           Panel8.Enabled := False;
           Panel8.BevelOuter := bvNone;
           Label9.Enabled := False;
           Panel10.Enabled:= False;
           Panel10.BevelOuter := bvNone;
         End;
    Else {bgTopBottom  bgBottomTop bgLeftToRight bgRightToLeft}
         Begin
           Label5.Enabled := False;
           Panel6.Enabled := False;
           Panel6.BevelOuter := bvNone;
           LAbel8.Enabled := True;
           Panel8.Enabled := True;
           Panel8.BevelOuter := bvLowered;
           Label9.Enabled := True;
           Panel10.Enabled:= True;
           Panel10.BevelOuter := bvLowered;
         End;
  End;

end;

procedure TForm1.Panel6Click(Sender: TObject);
// setting the graph's background color.
// This change is taken into account only if aGraph.backGround = bgColored;
begin
  ColorDialog1.Color := Panel6.Color;
  If ColorDialog1.Execute Then
  Begin
    QSpiderGraph1.backGroundColor := ColorDialog1.Color;
    Panel6.Color := ColorDialog1.Color;
  End;
end;

procedure TForm1.Panel8Click(Sender: TObject);
begin
  ColorDialog1.Color := Panel8.Color;
  If ColorDialog1.Execute Then
  Begin
    QSpiderGraph1.backGStartColor := ColorDialog1.Color;
    Panel8.Color := ColorDialog1.Color;
  End;
end;

procedure TForm1.Panel10Click(Sender: TObject);
begin
  ColorDialog1.Color := Panel10.Color;
  If ColorDialog1.Execute Then
  Begin
    QSpiderGraph1.backGFinalColor := ColorDialog1.Color;
    Panel10.Color := ColorDialog1.Color;
  End;
end;

procedure TForm1.Panel12Click(Sender: TObject);
// Background color of the box showing lines captions together with
// their colors.
begin
  ColorDialog1.Color := Panel12.Color;
  If ColorDialog1.Execute Then
  Begin
    QSpiderGraph1.linesBoxColor := ColorDialog1.Color;
    Panel12.Color := ColorDialog1.Color;
  End;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
// Shall the box showing lines captions and their colors be transparent ?
// If No, it will display the "linesBoxColor" decided above.
begin
  QSpiderGraph1.linesBoxTransparent := CheckBox3.Checked;
end;

procedure TForm1.CheckBox4Click(Sender: TObject);
// Showing or not the box containing the lines captions together with a little
// rectangle with their color.
begin
  QSpiderGraph1.showLinesCaption := CheckBox4.Checked;
end;

procedure TForm1.SpinEdit2Change(Sender: TObject);
// Lines' pen default width (global property. Each line's pen width coud be set
// individually too, using myGraph.lines[i].penWidth := ii;
begin
  QSpiderGraph1.defaultPenWidth := SpinEdit2.Value;
end;

procedure TForm1.CheckBox14Click(Sender: TObject);
// anchors property
begin
  If CheckBox14.Checked
     Then QSpiderGraph1.Anchors := [akLeft,akTop,akRight,akBottom]
     Else QSpiderGraph1.Anchors := [akLeft,akTop];
end;

procedure TForm1.Label10Click(Sender: TObject);
// (Lable10 contains the two lines caption of the CheckBox14 above.)
begin
  CheckBox14.Checked := Not(CheckBox14.Checked);
end;

// ---------------------------------------------------------------
// --- Tracking mouse
// ---------------------------------------------------------------

procedure TForm1.CheckBox5Click(Sender: TObject);
// General interuptor. If set to false, MousEnterLine and MouseExitLine
// will no longer be raised...
begin
  QSpiderGraph1.trackMouseMoves := CheckBox5.Checked;
  Checkbox6.Enabled := CheckBox5.Checked;
  CheckBox13.Enabled:= CheckBox5.Checked;
  Label17.Enabled   := CheckBox5.Checked;
  Label18.Enabled   := CheckBox5.Checked;
  Label19.Enabled   := CheckBox5.Checked;
  Panel16.Enabled   := CheckBox5.Checked;
  If CheckBox5.Checked
     Then Panel16.BevelOuter := bvLowered
     Else Panel16.BevelOuter := bvNone;
  CheckBox7.Enabled := CheckBox5.Checked;
  Label16.Enabled   := CheckBox5.Checked;
  Panel14.Enabled   := CheckBox5.Checked;
  If CheckBox5.Checked
     Then Panel14.BevelOuter := bvLowered
     Else Panel14.BevelOuter := bvNone;
end;

procedure TForm1.CheckBox6Click(Sender: TObject);
// Still track mouse moves (if ".trackMouseMoves" above is true),
// but don't display the lines info box near mouse pointer :
begin
  QSpiderGraph1.showMouseBox := CheckBox6.Checked;
end;

procedure TForm1.CheckBox7Click(Sender: TObject);
// The lines info box above will use either the lines colors as fore color,
// or a fixed one; (If "False", it uses ".mBoxForColor" below)
begin
  QSpiderGraph1.mBoxUsesLnColor := CheckBox7.Checked;
end;

procedure TForm1.Panel14Click(Sender: TObject);
// ... see above !
begin
  ColorDialog1.Color := Panel14.Color;
  If ColorDialog1.Execute Then
  Begin
    QSpiderGraph1.mBoxForColor := ColorDialog1.Color;
    Panel14.Color := ColorDialog1.Color;
  End;
end;

procedure TForm1.Panel16Click(Sender: TObject);
// background color of the box showing lines info box, near to mouse pointer
begin
  ColorDialog1.Color := Panel16.Color;
  If ColorDialog1.Execute Then
  Begin
    QSpiderGraph1.mBoxBackColor := ColorDialog1.Color;
    Panel16.Color := ColorDialog1.Color;
  End;
end;

procedure TForm1.CheckBox13Click(Sender: TObject);
// Assigning the form as parent to the boxes showing info-lines allows them
// to show on the whole form. Otherwise, (if Nil is passed), the line won't go
// beyond the limits of the graph.
begin
  If CheckBox13.Checked
     Then QSpiderGraph1.mBoxParent := Self
     Else QSpiderGraph1.mBoxParent := Nil;
end;

procedure TForm1.QSpiderGraph1MouseEnterLine(Sender: TObject;
  const lineIndex: Integer);
begin
  Memo1.Lines.Add('event "onMouseEnterLine". lineIndex=' + intToStr(lineIndex));
end;

procedure TForm1.QSpiderGraph1MouseExitLine(Sender: TObject;
  const lineIndex: Integer);
begin
  Memo1.Lines.Add('event "onMouseExitLine". lineIndex=' + intToStr(lineIndex));
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  Memo1.Clear;
end;


// ---------------------------------------------------------------
// --- Lines management
// ---------------------------------------------------------------

procedure TForm1.Button2Click(Sender: TObject);
// Using QSGraph's "AddLine();" function to add a line to the collection :

var anArray : TQSingleArray;//array of single, declared in TQSPiderGrpahU.pas;
    i: Integer;
    gtc : Real;

begin
  // As long as we're in a demo, colors are not the essential point. So we'll
  // just assign a random color to each new line. This will be done using "gtc"
  // as seed for the random() fct.
  gtc := GetTickCount64;


  // TplSpiderGraph waits for you to send lines values as single datas in an array
  // of kind TQSingleArray. It's length should be equal to the number of axes...
  SetLength(anArray,QSpiderGraph1.axesCount);

  // Like for colors, lines values don't count much in this demo, so let's chose them randomly
  randomize;
  For i := 0 To QSpiderGraph1.axesCount -1 Do
  Begin
    anArray[i] := Random(100);     // get a random value between this axe limits ([0..100])
  End;

  // i no longer needed, so reuse it to get the index of this new line.
  // This index will allow us to later set some of this new line's properties
  i := QSpiderGraph1.AddLine(anArray);

  // If a problem occured, the returned index = "-1"; Otherwise, set color and caption :
  If i < 0 Then ShowMessage ('Unable to add this line...')
  Else Begin
         QSpiderGraph1.lines[i].color  := TColor( Random( Round(gtc) Mod High(TColor)));
         QSpiderGraph1.lines[i].caption := 'Line N° ' + IntToStr(i+1);
       End;
End;

procedure TForm1.Button3Click(Sender: TObject);
// Removing a line is done easily in one step. But the line is definitely
// removed from the graph. If you want to temporarily "hide" a line instead,
// use the property "visible", or the procedure "Hide;" and "Show;" of each
// line. ( myGraph.Line[i].Hide )

begin
  // Makes sure the index to remove (=the contents of Edit2.text) is numeric :
  Try StrToInt(Edit2.Text); Except Edit2.Text := '0'; End;
  // Then, asks the graph to remove this line. The validity of the index will
  // be checked by the graph itself.
  QSpiderGraph1.RemoveLine(StrToInt(Edit2.Text));
end;

procedure TForm1.CheckBox8Click(Sender: TObject);
// showLinesPoints is a public property (run-time only) ;
begin
  QSpiderGraph1.showLinesPoints := CheckBox8.Checked;
end;

//  graph function    GetBestLineByArea : TIntegerDynArray;
//  graph function    GetBestLineByAxe(axeIx:Integer) : TIntegerDynArray;
//
//  Both returns an array containing 0..fLineCount-1 values,
//  beeing the indexes of the lines with the best areas/values;
//
// CARE : if no line has been set until now, the result will be -1 :

procedure TForm1.Button4Click(Sender: TObject);
// Retrieve the list of lines index with the widest area
var aDynArray:TIntegerDynArray;    // Will store the result ; declared in "Types"
    i : Integer;
    s : String;
begin
  If radioButton6.Checked Then s := 'Widest(s)areas'' indexe(s) : '
                          Else s := 'Narrowest areas'' indexe(s) : ';

  // If the function receives "True", it will search for the best <=> highest
  // value(s). Otherwise it will search the lowest one(s).
  aDynArray := QSpiderGraph1.GetBestLineByArea(radioButton6.Checked);

  // If a problem occured, the returned array first entry = "-1";
  If aDynArray[0] = -1
  Then ShowMessage('There is no line to sort (or index out of bounds)...')
  Else Begin
         For i := 0 To Length(aDynArray) -1 Do
             s := s + IntToStr(aDynArray[i])
                    + ' ("' + QSpiderGraph1.lines[aDynArray[i]].caption + '") ; ' ;
         ShowMessage(s);
       End;
end;

procedure TForm1.Button5Click(Sender: TObject);
// Retrieve the list of lines indexes with higher values on a given axe
var aDynArray:TIntegerDynArray;
    i : Integer;
    s,s2 : String;
begin
  // Checks that the index sent is correct
  TRY StrToInt(Edit3.Text); EXCEPT Edit3.Text := '0'; END;
  If RadioButton6.Checked Then s2 := 'highest'
                          Else s2 := 'lowest';
  s := 'Indexes of lines with '+s2+' values, for axe index [' + Edit3.Text +'] : ';

  // First parmeter being the index of an axe, it has to be >0 and < axesCount-1
  // The second parameter is a boolean. If its value is "True", the function will
  // consider "best" to min "highest", otherwise it wil search the "lowest" value(s).
  aDynArray := QSpiderGraph1.GetBestLineByAxe(StrToInt(Edit3.Text), radioButton6.Checked);

  // If a problem occured, the returned array first entry = "-1";
  If aDynArray[0] = -1
  Then ShowMessage('None of the lines has values (or index out of bounds)...')
  Else Begin
         For i := 0 To Length(aDynArray) -1 Do
             s := s + IntToStr(aDynArray[i])
                    + ' ("' + QSpiderGraph1.lines[aDynArray[i]].caption + '") ; ' ;
         ShowMessage(s);
       End;
end;

procedure TForm1.Panel18Click(Sender: TObject);
// The graph's "highlightColor" is used to highlight and/or flash lines.
begin
  ColorDialog1.Color := Panel18.Color;
  If ColorDialog1.Execute Then
  Begin
    QSpiderGraph1.highlightColor := ColorDialog1.Color;
    Panel18.Color := ColorDialog1.Color;
  End;
end;

procedure TForm1.CheckBox9Click(Sender: TObject);
// setting highligh mode
begin
  With QSpiderGraph1 Do
  Begin
    If CheckBox9 .Checked
       Then QSpiderGraph1.highlightMode := QSpiderGraph1.highlightMode + [hmShowPoints]
       Else QSpiderGraph1.highlightMode := QSpiderGraph1.highlightMode - [hmShowPoints];
    If CheckBox10.Checked
       Then QSpiderGraph1.highlightMode := QSpiderGraph1.highlightMode + [hmWidened]
       Else QSpiderGraph1.highlightMode := QSpiderGraph1.highlightMode - [hmWidened];
    If CheckBox11.Checked
       Then QSpiderGraph1.highlightMode := QSpiderGraph1.highlightMode + [hmColorLine]
       Else QSpiderGraph1.highlightMode := QSpiderGraph1.highlightMode - [hmColorLine];
   If CheckBox12.Checked
       Then QSpiderGraph1.highlightMode := QSpiderGraph1.highlightMode + [hmFlashLine]
       Else QSpiderGraph1.highlightMode := QSpiderGraph1.highlightMode - [hmFlashLine];
  End;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
// highlighting the requested line :
// -2 or HC_AREA = Reset all the lines
// -1 or HC_NONE = show the one with the widest area
// 0 .. axesCount -1 = show the one whose value on this axe is the greatest

// The second parameter for the function "HighlightLineByCrit();" is a boolean.
// If it equals TRUE, the function will search for the highest values (axes or area)
// If it equals FALSE, the function will search the lowest ones.
// Has you may see in the procedure after this one, the second parameter is
// optional. It's default value is TRUE (for "best <=> highest or widest").

// As you can see, the axe number isn't checked. If the one sent is above (axesCount - 1)
// or below HC_AREA, it simply will be converted by the graph to "HC_NONE".

var crit : integer;
begin
// If "by line index", call the appropriate function (one of the two overloaded definitions)
  If radioButton8.Checked Then
  Begin
    qspiderGraph1.HighlightLineByIndex(spinEdit3.Value);
    Exit;
  End;
// Otherwise, check what is wanted, then call the function with appropriate parm
  If radioButton3.Checked Then crit := HC_NONE              // <=> -1
  Else If radioButton4.Checked Then crit := HC_AREA         // <=> -2
  Else crit := spinEdit3.Value;
  qspiderGraph1.HighlightLineByCrit(crit,radioButton6.Checked);
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
// reseting the lines appearance, after a "sort"
begin
  qspiderGraph1.HighlightLineByCrit(HC_NONE);
end;

procedure TForm1.Button8Click(Sender: TObject);
// Changing lines captions box's font.
begin
  Try FontDialog3.Font := QSpiderGraph1.linesCpnFont ; Except; End;
  If FontDialog3.Execute Then
  Begin
    QSpiderGraph1.linesCpnFont := FontDialog3.Font;
    // Invalidate would change nothing now : As this property has no setter,
    // the graph has no mean to know that a change is to be taken into account
    // (The "box" is a bitmap computed as least as possible).
    // An easy workaround is to force the invalidation of the lines captions box,
    // by simply changing and resetting the linesBoxTransparent property, which
    // will invalidate the box.
    // (if your line box caption is not intended to change at run-time, setting it
    //  before sendind lines for example would be far enough, and the lines bellow
    //  would be unnecessary).
    With QSpiderGraph1 Do
    Begin
      // force the lineBox to be computed again
      linesBoxTransparent := Not(linesBoxTransparent);
      // and reset it in it's previous state
      linesBoxTransparent := Not(linesBoxTransparent);
      Invalidate;
    End;
  End;
end;

// V1.1 : Added image saving capabilities.
procedure TForm1.Button9Click(Sender: TObject);
// -1- As bitmap, 32 bits
begin
  If (QSpiderGraph1.Save2File('QSGSave2FileDemo.bmp') = True)
  Then ShowMessage ('Done')
  Else ShowMessage ('Failed');
end;

procedure TForm1.Button10Click(Sender: TObject);
// -2- As jpeg, low quality
begin
  If (QSpiderGraph1.Save2File('QSGSave2FileDemo.jpg',False) = True)
  Then ShowMessage ('Done')
  Else ShowMessage ('Failed');
end;

// -3- Into a TBitmap, provided by the programmer, who can do whatever
//     he wants with it when the function returns.

// In order to use this function, we need to instanciate a TBitmap, and pass
// it to the component.
// Characteristics like PixelFormat can be setup before the call if needed.
// Once the function has returned, do whatever you want with the bitmap object.

// The code to use would look like :

// procedure TForm1.Button_XXX_Click(Sender: TObject);
// var aBitmap: TBitmap;
// begin
//   aBitmap := TBitmap.Create;
//   aBitmap.PixelFormat := pf16bit; //pf4bit, pf8bit,pf15bit, pf16bit, pf24bit, pf32bit,
//   If QSpiderGraph1.Copy2Bitmap(aBitmap)
//      Then aBitmap.SaveToFile('FromCopy.bmp')
//      Else showMessage('Unable to make the copy');
// end;

initialization
  {$I mwin.lrs}

end.
