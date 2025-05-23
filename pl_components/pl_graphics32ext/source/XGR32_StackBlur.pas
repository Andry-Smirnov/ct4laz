
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)
***********************************************************************}

unit XGR32_StackBlur;

interface

uses
  SysUtils, classes, Math, GR32;

type

TStackBlur = class
  public
    fradius    : integer;
    constructor Create(radius: integer);
    procedure Execute(bitmap: TBitmap32);
  end;

implementation

type
  TInt3Array = array[0..2] of integer;
  PInt3Array = ^TInt3Array;

constructor TStackBlur.Create(radius: integer);
begin
  self.fradius := min(max(1,radius),40);
end;

procedure TStackBlur.Execute(bitmap: TBitmap32);
var
  i, w, h, wm, hm, wh, diam, q1, q2: integer;
  asum, rsum, gsum, bsum, x1, y1, yp, yi, yw: integer;
  pix: PColor32Array;
  p, p1, p2: TColor32Entry;
  a, r, g, b, vmin, vmax, dv: array of integer;
begin
  pix := bitmap.bits;
  w := bitmap.width;
  h := bitmap.height;
  wm := w -1;
  hm := h -1;
  wh := w * h;
  diam := fradius+fradius+1;
  setlength(a, wh);
  setlength(r, wh);
  setlength(g, wh);
  setlength(b, wh);
  setlength(vmin, max(w,h));
  setlength(vmax, max(w,h));
  setlength(dv, 256*diam);
  for i :=0 to 256*diam -1 do dv[i] := (i div diam);

  yw := 0;
  yi := 0;

  for y1 := 0 to h -1 do
  begin
    asum := 0; rsum := 0; gsum := 0; bsum := 0;
    for i := -fradius to fradius do
    begin
      {$R-}
      p.ARGB := pix^[yi + min(wm, max(i,0))];
      {$R+}
      inc(asum, p.A);
      inc(rsum, p.R);
      inc(gsum, p.G);
      inc(bsum, p.B);
    end;
    for x1 := 0 to w -1 do
    begin
      a[yi] := dv[asum];
      r[yi] := dv[rsum];
      g[yi] := dv[gsum];
      b[yi] := dv[bsum];

      if (y1 = 0) then
      begin
        vmin[x1] := min(x1+fradius+1, wm);
        vmax[x1] := max(x1-fradius, 0);
      end;

      {$R-}
      p1.ARGB := pix^[yw +vmin[x1]];
      p2.ARGB := pix^[yw +vmax[x1]];
      {$R+}

      inc(asum, p1.A - p2.A);
      inc(rsum, p1.R - p2.R);
      inc(gsum, p1.G - p2.G);
      inc(bsum, p1.B - p2.B);
      inc(yi);
    end;
    inc(yw, w);
  end;

  for x1 :=0 to w -1 do
  begin
    asum := 0; rsum := 0; gsum := 0; bsum := 0;
    yp := -fradius*w;
    for i := -fradius to fradius do
    begin
      yi := max(0, yp) +x1;
      inc(asum, a[yi]);
      inc(rsum, r[yi]);
      inc(gsum, g[yi]);
      inc(bsum, b[yi]);
      inc(yp, w);
    end;
    yi := x1;
    for y1 := 0 to h -1 do
    begin
      {$R-}
      pix^[yi] := cardinal((dv[asum] shl 24) or
        (dv[rsum] shl 16) or (dv[gsum] shl 8) or dv[bsum]);
      {$R+}
      if (x1 = 0) then
      begin
        vmin[y1] := min(y1 +fradius +1, hm) *w;
        vmax[y1] := max(y1 -fradius,0) *w;
      end;
      q1 := x1 +vmin[y1];
      q2 := x1 +vmax[y1];

      inc(asum, a[q1]-a[q2]);
      inc(rsum, r[q1]-r[q2]);
      inc(gsum, g[q1]-g[q2]);
      inc(bsum, b[q1]-b[q2]);

      inc(yi, w);
    end;
  end;

end;

end.
