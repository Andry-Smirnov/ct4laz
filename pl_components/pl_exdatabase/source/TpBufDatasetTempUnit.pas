unit TpBufDatasetTempUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BufDataset;

type
  TpBufDatasetTemp = class(TBufDataset)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

constructor TpBufDatasetTemp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TpBufDatasetTemp.Destroy;
begin
  inherited Destroy;
end;

end.
