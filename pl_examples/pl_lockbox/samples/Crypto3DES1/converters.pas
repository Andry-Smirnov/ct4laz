unit converters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TDataBytes = array of Byte;

  function StringToBytes(rida:string):TDataBytes;

implementation

function StringToBytes(rida:string):TDataBytes;
var
  //rida:string;
  vaherida:string;
  arv:integer;
  pikkus:integer;
  vahe:integer;
  vahealgus:integer;
  cmd:integer;
  Databloc2mode:boolean;
  Data:integer;

  Mitmes:integer;
begin
  vahe:=AnsiPos(';',rida) ;
   if vahe>0 then
    begin
      rida:=copy(rida,0,vahe-1);
    end;

  vahe:=AnsiPos('/',rida) ;
   if vahe>0 then
    begin
      rida:=copy(rida,0,vahe-1);
    end;
  vahe:=AnsiPos(':',rida) ;
   if vahe>0 then
    begin
      rida:=copy(rida,vahe+1,length(rida));
    end;



    //rida:=s;
  vahe:=0 ;
  vahealgus:=0;
  pikkus:=Length(rida);
  mitmes:=0;
  setlength(result,pikkus);
  Databloc2mode:=false;
  while vahe<=pikkus do
  begin
    if IsDelimiter(#32,rida,vahe)=true then
    begin
      vaherida:=trim(copy(rida,vahealgus,vahe-vahealgus));

      vahealgus:=vahe;
      if length(vaherida)>0 then
      begin

        if TryStrToInt(vaherida,arv)=false then raise exception.Create('Error convert TryStrToIntExt "'+vaherida+'"');
        result[mitmes]:=arv;
        inc(mitmes);
      end;
    end     else
    begin
      if vahe=pikkus then
      begin
      vaherida:=trim(copy(rida,vahealgus,vahe-vahealgus+1));
      if length(vaherida)>0 then
      begin
        if TryStrToInt(vaherida,arv)=false then raise exception.Create('Error convert TryStrToIntExt "'+vaherida+'"');
        result[mitmes]:=lo(arv);
        inc(mitmes);
      end;
      end;
    end;
    inc(vahe);
  end;

  setlength(result,mitmes);

end;

end.

