unit constvalues;

{$mode delphi}

interface

uses
  Classes, SysUtils;

const
  // Different platforms store resource files on different locations
{$IFDEF Windows}
    pathMedia = '..\xmedia\database1\';
{$ENDIF}
{$IFDEF UNIX}
    pathMedia = '../xmedia/database1/';
{$ENDIF}

implementation

end.

