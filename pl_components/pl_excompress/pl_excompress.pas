{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit pl_ExCompress;

{$warn 5023 off : no warning about unused units}
interface

uses
  AllExCompressRegister, compressbase, TplLzmaUnit, TplZipUnit, TplZlibUnit, 
  UBitTreeDecoder, UBitTreeEncoder, UBufferedFS, UCRC, ULZBinTree, 
  ULZInWindow, ULZMABase, ULZMABench, ULZMACommon, ULZMADecoder, ULZMAEncoder, 
  ULZOutWindow, URangeDecoder, URangeEncoder, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('AllExCompressRegister', @AllExCompressRegister.Register);
end;

initialization
  RegisterPackage('pl_ExCompress', @Register);
end.
