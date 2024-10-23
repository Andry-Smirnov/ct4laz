unit mainform;

interface

uses
  Messages, LResources, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TplMultiGraphUnit, StdCtrls, ExtCtrls, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button2: TButton;
    Image1:    TImage;
    Label12:   TLabel;
    tmrTest:   TTimer;
    btnStartStopTest: TButton;
    Image2:    TImage;
    Image3:    TImage;
    Image4:    TImage;
    Image5:    TImage;
    Image6:    TImage;
    GroupBox1: TGroupBox;
    Label3:    TLabel;
    Label4:    TLabel;
    Label5:    TLabel;
    Label6:    TLabel;
    Label7:    TLabel;
    Label1:    TLabel;
    Label8:    TLabel;
    Label9:    TLabel;
    Label10:   TLabel;
    Label11:   TLabel;
    Label15:   TLabel;
    Label14:   TLabel;
    GroupBox2: TGroupBox;
    Label16:   TLabel;
    Label17:   TLabel;
    Label18:   TLabel;
    Label19:   TLabel;
    Label20:   TLabel;
    Label21:   TLabel;
    Label22:   TLabel;
    Label23:   TLabel;
    Label24:   TLabel;
    Label25:   TLabel;
    Label26:   TLabel;
    Label27:   TLabel;
    GroupBox3: TGroupBox;
    Label2:    TLabel;
    Label28:   TLabel;
    Label29:   TLabel;
    Label30:   TLabel;
    Label31:   TLabel;
    Label32:   TLabel;
    Label33:   TLabel;
    Label34:   TLabel;
    Label35:   TLabel;
    Label36:   TLabel;
    Label37:   TLabel;
    Label38:   TLabel;
    GroupBox4: TGroupBox;
    Label39:   TLabel;
    Label40:   TLabel;
    Label41:   TLabel;
    Label42:   TLabel;
    Label43:   TLabel;
    Label44:   TLabel;
    Label45:   TLabel;
    Label46:   TLabel;
    Label47:   TLabel;
    Label48:   TLabel;
    Label49:   TLabel;
    Label50:   TLabel;
    GroupBox5: TGroupBox;
    Label51:   TLabel;
    Label52:   TLabel;
    Label53:   TLabel;
    Label54:   TLabel;
    Label55:   TLabel;
    Label56:   TLabel;
    Label57:   TLabel;
    Label58:   TLabel;
    Label59:   TLabel;
    Label60:   TLabel;
    Label61:   TLabel;
    Label62:   TLabel;
    GroupBox6: TGroupBox;
    Label63:   TLabel;
    Label64:   TLabel;
    Label65:   TLabel;
    Label66:   TLabel;
    Label67:   TLabel;
    Label68:   TLabel;
    Label69:   TLabel;
    Label70:   TLabel;
    Label71:   TLabel;
    Label72:   TLabel;
    Label73:   TLabel;
    Label74:   TLabel;
    Button4:   TButton;
    Button1:   TButton;
    dlgColor:  TColorDialog;
    pnlRAG1GridColor: TPanel;
    pnlRAG1BackColor: TPanel;
    pnlRAG1GraphColor: TPanel;
    procedure Button2Click(Sender: TObject);
    procedure tmrTestTimer(Sender: TObject);
    procedure btnStartStopTestClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure pnlRAG1BackColorClick(Sender: TObject);
    procedure pnlRAG1GridColorClick(Sender: TObject);
    procedure pnlRAG1GraphColorClick(Sender: TObject);
  private
    _graph: TRAGGraph;
    _rag:   TRAG;
    _rag2:  TRAG;
    _rag3:  TRAG;
    _rag4:  TRAG;
    _rag5:  TRAG;
    _rag6:  TRAG;
    _rag1:  TRAG;
  public

  end;

var
  Form1: TForm1;

implementation

procedure TForm1.FormCreate(Sender: TObject);
begin
  ///////////1. RAG///////////////////
  _rag := TRAG.Create(image1.Width, image1.Height);
  _rag.GridStepHorz := 10;
  _rag.GridStepVert := 40;
  _rag.XScalar := 2;

  _graph := TRAGGraph.Create(125);
  _graph.Color := clBlue;
  _graph.GraphType := gtLines;
  _rag.AddGraph(_graph);

  _rag.RedrawGraphs(True);
  image1.Picture := _rag.Picture;

  ///////////2. RAG///////////////////
  _rag2 := TRAG.Create(image2.Width, image2.Height);
  _rag2.GridStepHorz := 20;
  _rag2.GridStepVert := 40;
  _rag2.AddGraph(65, clGreen, gtArea);
  _rag2.XScalar := 4;
  _rag2.TextBackground := True;
  _rag2.RedrawGraphs(True);
  image2.Picture := _rag2.Picture;


  ///////////3. RAG///////////////////
  _rag3 := TRAG.Create(image3.Width, image3.Height);
  _rag3.GridStepHorz := 10;
  _rag3.GridStepVert := 10;
  _rag3.BackColor := clBlack;
  _rag3.GridColor := clGreen;

  _rag3.AddGraph(125, clLime, gtLines);
  _rag3.Graphs[0].LineWidth := 1;
  _rag3.AddGraph(125, clYellow, gtLines);
  _rag3.Graphs[1].LineWidth := 2;
  _rag3.AddValue(1, 40);
  _rag3.TextBackground := True;
  _rag3.TextColor      := clLime;
  _rag3.TextFont.Style := [fsBold, fsItalic];

  _rag3.XScalar := 2;
  _rag3.RedrawGraphs(True);
  image3.Picture := _rag3.Picture;


  ///////////4. RAG///////////////////
  _rag4 := TRAG.Create(image4.Width, image4.Height);
  _rag4.GridStepHorz := 10;
  _rag4.GridStepVert := 10;
  _rag4.ValueOffsetPercentage := 10;
  _rag4.AddGraph(125, clGreen, gtBarsRelative);
  _rag4.Graphs[0].UpperThreshold := 60;
  _rag4.Graphs[0].LowerThreshold := 30;

  _rag4.XScalar := 2;
  _rag4.RedrawGraphs(True);
  image4.Picture := _rag4.Picture;


  ///////////5. RAG///////////////////
  _rag5 := TRAG.Create(image5.Width, image5.Height);
  _rag5.GridStepHorz := 10;
  _rag5.GridStepVert := 10;
  _rag5.TextBackground := True;

  _rag5.AddGraph(125, clGreen, gtLines);
  _rag5.AddGraph(125, clRed, gtLines);
  _rag5.AddGraph(125, clYellow, gtLines);
  _rag5.AddGraph(125, clBlue, gtLines);

  _rag5.XScalar := 2;
  _rag5.RedrawGraphs(True);
  image5.Picture := _rag5.Picture;


  ///////////6. RAG///////////////////
  _rag6 := TRAG.Create(image6.Width, image6.Height);
  _rag6.GridStepHorz := 20;
  _rag6.GridStepVert := 50;

  _rag6.AddGraph(125, clGreen, gtBarsAbsolute);
  _rag6.Graphs[0].LowerThreshold := 100;
  _rag6.Graphs[0].UpperThreshold := 200;

  _rag6.XScalar := 2;
  _rag6.RedrawGraphs(True);
  image6.Picture  := _rag6.Picture;
  Button4.Enabled := True;
end;

procedure TForm1.tmrTestTimer(Sender: TObject);
var
  neg: integer;
var
  val: int64;
begin
  Randomize();


  _rag.AddValue(0, Random(200));
  _rag.TextRightTop    := IntToStr(_rag.Graphs[0].MaxValue);
  _rag.TextRightBottom := IntToStr(_rag.Graphs[0].Length);
  _rag2.AddValues([Random(100), Random(100)]);
  _rag2.TextRightTop    := IntToStr(_rag2.Graphs[0].MaxValue);
  _rag2.TextRightBottom := IntToStr(_rag2.Graphs[0].Length);

  neg := Random(100);
  if (neg > 50) then
  begin
    neg := 1;
  end
  else
  begin
    neg := -1;
  end;

  val := _rag3.Graphs[1].Values.GetData(_rag3.Graphs[1].Values.Count - 1) +
    Random(2) * neg;

  if (val > 100) then
  begin
    val := 100;
  end
  else if (val < 0) then
  begin
    val := 0;
  end;

  _rag3.AddValues([Random(80), val]);
  _rag3.TextRightTop    := IntToStr(_rag3.Graphs[0].MaxValue);
  _rag3.TextRightBottom := IntToStr(_rag3.Graphs[0].Length);
  _rag4.AddValues([Random(80)]);
  _rag5.AddValues([Random(80), Random(40), Random(60), Random(20)]);
  _rag6.AddValues([Random(300)]);

  //recalculate and redraw the graphs (parameter: true)
  _rag.RedrawGraphs(True);
  _rag2.RedrawGraphs(True);
  _rag3.RedrawGraphs(True);
  _rag4.RedrawGraphs(True);
  _rag5.RedrawGraphs(True);
  _rag6.RedrawGraphs(True);

  //show output
  image1.Picture := _rag.Picture;
  image2.Picture := _rag2.Picture;
  image3.Picture := _rag3.Picture;
  image4.Picture := _rag4.Picture;
  image5.Picture := _rag5.Picture;
  image6.Picture := _rag6.Picture;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  close;
end;

procedure TForm1.btnStartStopTestClick(Sender: TObject);
begin
  tmrTest.Enabled := not tmrTest.Enabled;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  neg: integer;
var
  val: int64;
begin
  self.tmrTestTimer(Sender);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  case _rag4.Graphs[0].GraphType of
    gtLines: _rag4.Graphs[0].GraphType := gtArea;
    gtArea: _rag4.Graphs[0].GraphType  := gtBarsAbsolute;
    gtBarsAbsolute: _rag4.Graphs[0].GraphType := gtBarsRelative;
    gtBarsRelative: _rag4.Graphs[0].GraphType := gtLines;
  end;
end;

procedure TForm1.pnlRAG1BackColorClick(Sender: TObject);
begin
  dlgColor.Color := pnlRAG1BackColor.Color;

  if (dlgColor.Execute()) then
  begin
    pnlRAG1BackColor.Color := dlgColor.Color;
    _rag.BackColor := dlgColor.Color;
    _rag.RedrawGraphs(False);
    image1.Picture := _rag.Picture;
  end;
end;

procedure TForm1.pnlRAG1GridColorClick(Sender: TObject);
begin
  dlgColor.Color := pnlRAG1GridColor.Color;

  if (dlgColor.Execute()) then
  begin
    pnlRAG1GridColor.Color := dlgColor.Color;
    _rag.GridColor := dlgColor.Color;
    _rag.RedrawGraphs(False);
    image1.Picture := _rag.Picture;
  end;
end;

procedure TForm1.pnlRAG1GraphColorClick(Sender: TObject);
begin
  dlgColor.Color := _rag.Graphs[0].Color;

  if (dlgColor.Execute()) then
  begin
    pnlRAG1GraphColor.Color := dlgColor.Color;
    _rag.Graphs[0].Color    := dlgColor.Color;
    _rag.RedrawGraphs(False);
    image1.Picture := _rag.Picture;
  end;
end;

initialization
  {$I mainform.lrs}

end.

