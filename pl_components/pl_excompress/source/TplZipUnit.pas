{**********************************************************************
 Package pl_ExCompress
 This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)
***********************************************************************}

{$mode objfpc}
{$h+}
unit TplZipUnit;

interface

uses
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  SysUtils, Classes,rtlconsts, zstream;

const
  { Signatures }
  END_OF_CENTRAL_DIR_SIGNATURE = $06054B50;
  ZIP64_END_OF_CENTRAL_DIR_SIGNATURE = $06064B50;
  ZIP64_END_OF_CENTRAL_DIR_LOCATOR_SIGNATURE = $07064B50;
  LOCAL_FILE_HEADER_SIGNATURE = $04034B50;
  CENTRAL_FILE_HEADER_SIGNATURE = $02014B50;
  ZIP64_HEADER_ID = $0001;
  // infozip unicode path
  INFOZIP_UNICODE_PATH_ID = $7075;

  OS_FAT = 0; //MS-DOS and OS/2 (FAT/VFAT/FAT32)
  OS_UNIX = 3;
  OS_OS2 = 6; //OS/2 HPFS
  OS_NTFS = 10;
  OS_VFAT = 14;
  OS_OSX = 19;

  UNIX_MASK = $F000;
  UNIX_FIFO = $1000;
  UNIX_CHAR = $2000;
  UNIX_DIR = $4000;
  UNIX_BLK = $6000;
  UNIX_FILE = $8000;
  UNIX_LINK = $A000;
  UNIX_SOCK = $C000;


  UNIX_RUSR = $0100;
  UNIX_WUSR = $0080;
  UNIX_XUSR = $0040;

  UNIX_RGRP = $0020;
  UNIX_WGRP = $0010;
  UNIX_XGRP = $0008;

  UNIX_ROTH = $0004;
  UNIX_WOTH = $0002;
  UNIX_XOTH = $0001;

  UNIX_DEFAULT = UNIX_RUSR or UNIX_WUSR or UNIX_XUSR or UNIX_RGRP or UNIX_ROTH;

  TABLESIZE = 8191;
  FIRSTENTRY = 257;

type
  CodeRec = packed record
    Child: smallint;
    Sibling: smallint;
    Suffix: byte;
  end;
  CodeArray = array[0..TABLESIZE] of CodeRec;
  TablePtr = ^CodeArray;

  FreeListPtr = ^FreeListArray;
  FreeListArray = array[FIRSTENTRY..TABLESIZE] of word;

  BufPtr = PByte;

  Local_File_Header_Type = packed record //1 per zipped file
    Signature: longint; //4 bytes
    Extract_Version_Reqd: word; //if zip64: >= 45
    Bit_Flag: word; //"General purpose bit flag in PKZip appnote
    Compress_Method: word;
    Last_Mod_Time: word;
    Last_Mod_Date: word;
    Crc32: longword;
    Compressed_Size: longword;
    Uncompressed_Size: longword;
    Filename_Length: word;
    Extra_Field_Length: word; //refers to Extensible data field size
  end;

  Extensible_Data_Field_Header_Type = packed record
    // Beginning of extra field
    // after local file header
    // after central directory header
    Header_ID: word;
    //e.g. $0001 (ZIP64_HEADER_ID) Zip64 extended information extra field
    //     $0009 OS/2: extended attributes
    //     $000a NTFS: (Win32 really)
    //     $000d UNIX: uid, gid etc
    Data_Size: word; //size of following field data
    //... field data should follow...
  end;

  Zip64_Extended_Info_Field_Type = packed record //goes after Extensible_Data_Field_Header_Type
    // overrides Local and Central Directory data
    // stored in extra field
    Original_Size: QWord; //Uncompressed file
    Compressed_Size: QWord; //Compressed data
    Relative_Hdr_Offset: QWord; //Offset that leads to local header record
    Disk_Start_Number: longword; //on which disk this file starts
  end;

  { Define the Central Directory record types }

  Central_File_Header_Type = packed record
    Signature: longint; //4 bytes
    MadeBy_Version: word; //if zip64: lower byte >= 45
    Extract_Version_Reqd: word; //if zip64: >=45
    Bit_Flag: word; //General purpose bit flag in PKZip appnote
    Compress_Method: word;
    Last_Mod_Time: word;
    Last_Mod_Date: word;
    Crc32: longword;
    Compressed_Size: longword;
    Uncompressed_Size: longword;
    Filename_Length: word;
    Extra_Field_Length: word;
    File_Comment_Length: word;
    Starting_Disk_Num: word;
    Internal_Attributes: word;
    External_Attributes: longword;
    Local_Header_Offset: longword; // if zip64: 0xFFFFFFFF
  end;

  End_of_Central_Dir_Type = packed record //End of central directory record
    //1 per zip file, near end, before comment
    Signature: longint; //4 bytes
    Disk_Number: word;
    Central_Dir_Start_Disk: word;
    Entries_This_Disk: word;
    Total_Entries: word;
    Central_Dir_Size: longword;
    Start_Disk_Offset: longword;
    ZipFile_Comment_Length: word;
  end;

  Zip64_End_of_Central_Dir_type = packed record
    Signature: longint;
    Record_Size: QWord;
    Version_Made_By: word; //lower byte >= 45
    Extract_Version_Reqd: word; //version >= 45
    Disk_Number: longword;
    Central_Dir_Start_Disk: longword;
    Entries_This_Disk: QWord;
    Total_Entries: QWord;
    Central_Dir_Size: QWord;
    Start_Disk_Offset: QWord;
  end;

  Zip64_End_of_Central_Dir_Locator_type = packed record //comes after Zip64_End_of_Central_Dir_type
    Signature: longint;
    Zip64_EOCD_Start_Disk: longword; //Starting disk for Zip64 End of Central Directory record
    Central_Dir_Zip64_EOCD_Offset: QWord; //offset of Zip64 End of Central Directory record
    Total_Disks: longword; //total number of disks (contained in zip)
  end;

  TProgressEvent = procedure(Sender: TObject; const Pct: double) of object;
  TProgressEventEx = procedure(Sender: TObject; const ATotPos, ATotSize: int64) of object;
  TOnEndOfFileEvent = procedure(Sender: TObject; const Ratio: double) of object;
  TOnStartFileEvent = procedure(Sender: TObject; const AFileName: string) of object;


TZipCompressor = class(TObject)
  private
    FTerminated: boolean;
  protected
    FInFile: TStream;        { I/O file variables                         }
    FOutFile: TStream;
    FCrc32Val: longword;       { CRC calculation variable                   }
    FBufferSize: longword;
    FOnPercent: integer;
    FOnProgress: TProgressEvent;
    procedure UpdC32(Octet: byte);
  public
    constructor Create(AInFile, AOutFile: TStream; ABufSize: longword); virtual;
    procedure Compress; virtual; abstract;
    class function ZipID: word; virtual; abstract;
    class function ZipVersionReqd: word; virtual; abstract;
    function ZipBitFlag: word; virtual; abstract;
    procedure Terminate;
    property BufferSize: longword read FBufferSize;
    property OnPercent: integer read FOnPercent write FOnPercent;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property Crc32Val: longword read FCrc32Val write FCrc32Val;
    property Terminated: boolean read FTerminated;
  end;


TZipDeCompressor = class(TObject)
  protected
    FInFile: TStream;        { I/O file variables                         }
    FOutFile: TStream;
    FCrc32Val: longword;       { CRC calculation variable                   }
    FBufferSize: longword;
    FOnPercent: integer;
    FOnProgress: TProgressEvent;
    FOnProgressEx: TProgressEventEx;
    FTotPos: int64;
    FTotSize: int64;
    FTerminated: boolean;
    procedure UpdC32(Octet: byte);
  public
    constructor Create(AInFile, AOutFile: TStream; ABufSize: longword); virtual;
    procedure DeCompress; virtual; abstract;
    procedure Terminate;
    class function ZipID: word; virtual; abstract;
    property BufferSize: longword read FBufferSize;
    property OnPercent: integer read FOnPercent write FOnPercent;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OnProgressEx: TProgressEventEx read FOnProgressEx write FOnProgressEx;
    property Crc32Val: longword read FCrc32Val write FCrc32Val;
    property Terminated: boolean read FTerminated;
  end;

TZipShrinker = class(TZipCompressor)
  private
    FBufSize: longword;
    MaxInBufIdx: longword;      { Count of valid chars in input buffer       }
    InputEof: boolean;       { End of file indicator                      }
    CodeTable: TablePtr;      { Points to code table for LZW compression   }
    FreeList: FreeListPtr;   { Table of free code table entries           }
    NextFree: word;          { Index into free list table                 }

    ClearList: array[0..1023] of byte;  { Bit mapped structure used in     }
    {    during adaptive resets        }
    CodeSize: byte;     { Size of codes (in bits) currently being written }
    MaxCode: word;   { Largest code that can be written in CodeSize bits }
    InBufIdx,                     { Points to next char in buffer to be read   }
    OutBufIdx: longword;      { Points to next free space in output buffer }
    InBuf,                        { I/O buffers                                }
    OutBuf: BufPtr;
    FirstCh: boolean;  { Flag indicating the START of a shrink operation }
    TableFull: boolean;  { Flag indicating a full symbol table             }
    SaveByte: byte;     { Output code buffer                              }
    BitsUsed: byte;     { Index into output code buffer                   }
    BytesIn: longword;  { Count of input file bytes processed             }
    BytesOut: longword;  { Count of output bytes                           }
    FOnBytes: longword;
    procedure FillInputBuffer;
    procedure WriteOutputBuffer;
    procedure FlushOutput;
    procedure PutChar(B: byte);
    procedure PutCode(Code: smallint);
    procedure InitializeCodeTable;
    procedure Prune(Parent: word);
    procedure Clear_Table;
    procedure Table_Add(Prefix: word; Suffix: byte);
    function Table_Lookup(TargetPrefix: smallint; TargetSuffix: byte;
      Out FoundAt: smallint): boolean;
    procedure Shrink(Suffix: smallint);
    procedure ProcessLine(const Source: string);
    procedure DoOnProgress(const Pct: double); virtual;
  public
    constructor Create(AInFile, AOutFile: TStream; ABufSize: longword); override;
    destructor Destroy; override;
    procedure Compress; override;
    class function ZipID: word; override;
    class function ZipVersionReqd: word; override;
    function ZipBitFlag: word; override;
  end;


TZipDeflater = class(TZipCompressor)
  private
    FCompressionLevel: TCompressionlevel;
  public
    constructor Create(AInFile, AOutFile: TStream; ABufSize: longword); override;
    procedure Compress; override;
    class function ZipID: word; override;
    class function ZipVersionReqd: word; override;
    function ZipBitFlag: word; override;
    property CompressionLevel: TCompressionlevel read FCompressionLevel write FCompressionLevel;
  end;


TZipInflater = class(TZipDeCompressor)
  public
    constructor Create(AInFile, AOutFile: TStream; ABufSize: longword); override;
    procedure DeCompress; override;
    class function ZipID: word; override;
  end;

TZipFileEntry = class(TCollectionItem)
  private
    FArchiveFileName: string;
    FUTF8FileName: UTF8String;
    FUTF8DiskFileName: UTF8String;
    FAttributes: longword;
    FDateTime: TDateTime;
    FDiskFileName: string;
    FHeaderPos: int64;
    FNeedsZip64: boolean;    //flags whether filesize is big enough so we need a zip64 entry
    FOS: byte;
    FSize: int64;
    FStream: TStream;
    FCompressionLevel: TCompressionlevel;
    function GetArchiveFileName: string;
    function GetUTF8ArchiveFileName: UTF8String;
    function GetUTF8DiskFileName: UTF8String;
    procedure SetArchiveFileName(const AValue: string);
    procedure SetDiskFileName(const AValue: string);
    procedure SetUTF8ArchiveFileName(AValue: UTF8String);
    procedure SetUTF8DiskFileName(AValue: UTF8String);
  protected
    // For multi-disk support, a disk number property could be added here.
    property HdrPos: int64 read FHeaderPos write FheaderPos;
    property NeedsZip64: boolean read FNeedsZip64 write FNeedsZip64;
  public
    constructor Create(ACollection: TCollection); override;
    function IsDirectory: boolean;
    function IsLink: boolean;
    procedure Assign(Source: TPersistent); override;
    property Stream: TStream read FStream write FStream;
  published
    //Name of the file as it appears in the zip file list
    property ArchiveFileName: string read GetArchiveFileName write SetArchiveFileName;
    property UTF8ArchiveFileName: UTF8String read GetUTF8ArchiveFileName write SetUTF8ArchiveFileName;

    //Name of the file on disk (i.e. uncompressed. Can be empty if based on a stream uses local OS/filesystem directory separators
    property DiskFileName: string read FDiskFileName write SetDiskFileName;
    property UTF8DiskFileName: UTF8String read GetUTF8DiskFileName write SetUTF8DiskFileName;

    property Size: int64 read FSize write FSize;
    property DateTime: TDateTime read FDateTime write FDateTime;
    property OS: byte read FOS write FOS;
    property Attributes: longword read FAttributes write FAttributes;
    property CompressionLevel: TCompressionlevel read FCompressionLevel write FCompressionLevel;
  end;

TZipFileEntries = class(TCollection)
  private
    function GetZ(AIndex: integer): TZipFileEntry;
    procedure SetZ(AIndex: integer; const AValue: TZipFileEntry);
  public
    function AddFileEntry(const ADiskFileName: string): TZipFileEntry;
    function AddFileEntry(const ADiskFileName, AArchiveFileName: string): TZipFileEntry;
    function AddFileEntry(const AStream: TSTream; const AArchiveFileName: string): TZipFileEntry;
    procedure AddFileEntries(const List: TStrings);
    property Entries[AIndex: integer]: TZipFileEntry read GetZ write SetZ; default;
  end;

TFullZipFileEntry = class(TZipFileEntry)
  private
    FBitFlags: word;
    FCompressedSize: QWord;
    FCompressMethod: word;
    FCRC32: longword;
  public
    property BitFlags: word read FBitFlags;
    property CompressMethod: word read FCompressMethod;
    property CompressedSize: QWord read FCompressedSize;
    property CRC32: longword read FCRC32 write FCRC32;
  end;

  TOnCustomStreamEvent = procedure(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry) of object;
  TCustomInputStreamEvent = procedure(Sender: TObject; var AStream: TStream) of object;

TFullZipFileEntries = class(TZipFileEntries)
  private
    function GetFZ(AIndex: integer): TFullZipFileEntry;
    procedure SetFZ(AIndex: integer; const AValue: TFullZipFileEntry);
  public
    property FullEntries[AIndex: integer]: TFullZipFileEntry read GetFZ write SetFZ; default;
  end;


TplZipCompress = class(TComponent)
  private
    FEntries: TZipFileEntries;
    FTerminated: boolean;
    FZipping: boolean;
    FBufSize: longword;
    FFileName: RawByteString;
    FFileComment: string;
    FFiles: TStrings;
    FInMemSize: int64;
    FZipFileNeedsZip64: boolean; //flags whether at least one file is big enough to require a zip64 record
    FOutStream: TStream;
    FInFile: TStream;            // I/O file variables
    LocalHdr: Local_File_Header_Type;
    LocalZip64ExtHdr: Extensible_Data_Field_Header_Type; //Extra field header fixed to zip64 (i.e. .ID=1)
    LocalZip64Fld: Zip64_Extended_Info_Field_Type;       //header is in LocalZip64ExtHdr
    CentralHdr: Central_File_Header_Type;
    EndHdr: End_of_Central_Dir_Type;
    FOnPercent: longint;
    FOnProgress: TProgressEvent;
    FOnEndOfFile: TOnEndOfFileEvent;
    FOnStartFile: TOnStartFileEvent;
    FCurrenZipCompressor: TZipCompressor;
    function CheckEntries: integer;
    procedure SetEntries(const AValue: TZipFileEntries);
  protected
    procedure CloseInput(Item: TZipFileEntry);
    procedure StartZipFile(Item: TZipFileEntry);
    function UpdateZipHeader(Item: TZipFileEntry; FZip: TStream; ACRC: longword; AMethod: word; AZipVersionReqd: word;
                             AZipBitFlag: word): boolean;
    procedure BuildZipDirectory; //Builds central directory based on local headers
    procedure DoEndOfFile;
    procedure ZipOneFile(Item: TZipFileEntry); virtual;
    function  OpenInput(Item: TZipFileEntry): boolean;
    procedure GetFileInfo;
    procedure SetBufSize(Value: longword);
    procedure SetFileName(Value: RawByteString);
    function CreateCompressor(Item: TZipFileEntry; AinFile, AZipStream: TStream): TZipCompressor; virtual;
    property NeedsZip64: boolean read FZipFileNeedsZip64 write FZipFileNeedsZip64;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ZipAllFiles; virtual;
    // Saves zip to file and changes FileName
    procedure SaveToFile(AFileName: RawByteString);
    // Saves zip to stream
    procedure SaveToStream(AStream: TStream);
    // Zips specified files into a zip with name AFileName
    procedure ZipFiles(AFileName: RawByteString; FileList: TStrings);
    procedure ZipFiles(FileList: TStrings);
    // Zips specified entries into a zip with name AFileName
    procedure ZipFiles(AFileName: RawByteString; Entries: TZipFileEntries);
    procedure ZipFiles(Entries: TZipFileEntries);
    procedure Clear;
    procedure Terminate;

    property BufferSize: longword read FBufSize write SetBufSize;
    // Name of resulting out Zip file
    property FileName: RawByteString read FFileName write SetFileName;
    property FileComment: string read FFileComment write FFileComment;
    property InMemSize: int64 read FInMemSize write FInMemSize;
    property Entries: TZipFileEntries read FEntries write SetEntries;
    property Terminated: boolean read FTerminated;

    property OnPercent: integer read FOnPercent write FOnPercent;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OnStartFile: TOnStartFileEvent read FOnStartFile write FOnStartFile;
    property OnEndFile: TOnEndOfFileEvent read FOnEndOfFile write FOnEndOfFile;
  end;


TplZipUnCompress = class(TComponent)
  private
    FOnCloseInputStream: TCustomInputStreamEvent;
    FOnCreateStream: TOnCustomStreamEvent;
    FOnDoneStream: TOnCustomStreamEvent;
    FOnOpenInputStream: TCustomInputStreamEvent;
    FUnZipping: boolean;
    FBufSize: longword;
    FFileName: RawByteString;
    FOutputPath: RawByteString;
    FFileComment: string;
    FEntries: TFullZipFileEntries;
    FFiles: TStrings;
    FUseUTF8: boolean;
    FZipStream: TStream;                           // I/O file variables
    LocalHdr: Local_File_Header_Type;              // Local header, before compressed file data
    LocalZip64Fld: Zip64_Extended_Info_Field_Type; // header is in LocalZip64ExtHdr
    CentralHdr: Central_File_Header_Type;
    FTotPos: int64;
    FTotSize: int64;
    FTerminated: boolean;
    FOnPercent: longint;
    FOnProgress: TProgressEvent;
    FOnProgressEx: TProgressEventEx;
    FOnEndOfFile: TOnEndOfFileEvent;
    FOnStartFile: TOnStartFileEvent;
    FCurrenZipDeCompressor: TZipDeCompressor;
    function CalcTotalSize(AllFiles: boolean): int64;
    function IsMatch(I: TFullZipFileEntry): boolean;
  protected
    procedure OpenInput;
    procedure CloseOutput(Item: TFullZipFileEntry; var OutStream: TStream);
    procedure CloseInput;
    procedure FindEndHeaders(out AEndHdr: End_of_Central_Dir_Type; out AEndHdrPos: int64;
                             out AEndZip64Hdr: Zip64_End_of_Central_Dir_type; out AEndZip64HdrPos: int64);
    procedure ReadZipDirectory;
    procedure ReadZipHeader(Item: TFullZipFileEntry; out AMethod: word);
    procedure DoEndOfFile;
    procedure UnZipOneFile(Item: TFullZipFileEntry); virtual;
    function  OpenOutput(OutFileName: RawByteString; Out OutStream: TStream; Item: TFullZipFileEntry): boolean;
    procedure SetBufSize(Value: longword);
    procedure SetFileName(Value: RawByteString);
    procedure SetOutputPath(Value: RawByteString);
    function CreateDeCompressor(Item: TZipFileEntry; AMethod: word; AZipFile, AOutFile: TStream): TZipDeCompressor; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UnZipAllFiles; virtual;
    procedure UnZipFiles(AFileName: RawByteString; FileList: TStrings);
    procedure UnZipFiles(FileList: TStrings);
    procedure UnZipAllFiles(AFileName: RawByteString);
    procedure Clear;
    procedure Examine;
    procedure Terminate;
  public
    property BufferSize: longword read FBufSize write SetBufSize;
    // In zip file
    property FileName: RawByteString read FFileName write SetFileName;
    property OutputPath: RawByteString read FOutputPath write SetOutputPath;
    property FileComment: string read FFileComment;
    property Files: TStrings read FFiles;
    property Entries: TFullZipFileEntries read FEntries;
    property UseUTF8: boolean read FUseUTF8 write FUseUTF8;
    property Terminated: boolean read FTerminated;

    property OnOpenInputStream: TCustomInputStreamEvent read FOnOpenInputStream write FOnOpenInputStream;
    property OnCloseInputStream: TCustomInputStreamEvent read FOnCloseInputStream write FOnCloseInputStream;
    property OnCreateStream: TOnCustomStreamEvent read FOnCreateStream write FOnCreateStream;
    property OnDoneStream: TOnCustomStreamEvent read FOnDoneStream write FOnDoneStream;
    property OnPercent: integer read FOnPercent write FOnPercent;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OnProgressEx: TProgressEventEx read FOnProgressEx write FOnProgressEx;
    property OnStartFile: TOnStartFileEvent read FOnStartFile write FOnStartFile;
    property OnEndFile: TOnEndOfFileEvent read FOnEndOfFile write FOnEndOfFile;
  end;

  EZipError = class(Exception);

implementation

resourcestring
  SErrBufsizeChange = 'Changing buffer size is not allowed while (un)zipping.';
  SErrFileChange = 'Changing output file name is not allowed while (un)zipping.';
  SErrInvalidCRC = 'Invalid CRC checksum while unzipping %s.';
  SErrCorruptZIP = 'Corrupt ZIP file %s.';
  SErrUnsupportedCompressionFormat = 'Unsupported compression format %d.';
  SErrUnsupportedMultipleDisksCD = 'A central directory split over multiple disks is unsupported.';
  SErrMaxEntries = 'Encountered %d file entries; maximum supported is %d.';
  SErrMissingFileName = 'Missing filename in entry %d.';
  SErrMissingArchiveName = 'Missing archive filename in streamed entry %d.';
  SErrFileDoesNotExist = 'File "%s" does not exist.';
  SErrPosTooLarge = 'Position/offset %d is larger than maximum supported %d.';
  SErrNoFileName = 'No archive filename for examine operation.';
  SErrNoStream = 'No stream is opened.';
  SErrEncryptionNotSupported = 'Cannot unzip item "%s": encryption is not supported.';
  SErrPatchSetNotSupported = 'Cannot unzip item "%s": patch sets are not supported.';



const
  Crc_32_Tab: array[0..255] of longword = (
    $00000000, $77073096, $ee0e612c, $990951ba, $076dc419, $706af48f, $e963a535, $9e6495a3,
    $0edb8832, $79dcb8a4, $e0d5e91e, $97d2d988, $09b64c2b, $7eb17cbd, $e7b82d07, $90bf1d91,
    $1db71064, $6ab020f2, $f3b97148, $84be41de, $1adad47d, $6ddde4eb, $f4d4b551, $83d385c7,
    $136c9856, $646ba8c0, $fd62f97a, $8a65c9ec, $14015c4f, $63066cd9, $fa0f3d63, $8d080df5,
    $3b6e20c8, $4c69105e, $d56041e4, $a2677172, $3c03e4d1, $4b04d447, $d20d85fd, $a50ab56b,
    $35b5a8fa, $42b2986c, $dbbbc9d6, $acbcf940, $32d86ce3, $45df5c75, $dcd60dcf, $abd13d59,
    $26d930ac, $51de003a, $c8d75180, $bfd06116, $21b4f4b5, $56b3c423, $cfba9599, $b8bda50f,
    $2802b89e, $5f058808, $c60cd9b2, $b10be924, $2f6f7c87, $58684c11, $c1611dab, $b6662d3d,
    $76dc4190, $01db7106, $98d220bc, $efd5102a, $71b18589, $06b6b51f, $9fbfe4a5, $e8b8d433,
    $7807c9a2, $0f00f934, $9609a88e, $e10e9818, $7f6a0dbb, $086d3d2d, $91646c97, $e6635c01,
    $6b6b51f4, $1c6c6162, $856530d8, $f262004e, $6c0695ed, $1b01a57b, $8208f4c1, $f50fc457,
    $65b0d9c6, $12b7e950, $8bbeb8ea, $fcb9887c, $62dd1ddf, $15da2d49, $8cd37cf3, $fbd44c65,
    $4db26158, $3ab551ce, $a3bc0074, $d4bb30e2, $4adfa541, $3dd895d7, $a4d1c46d, $d3d6f4fb,
    $4369e96a, $346ed9fc, $ad678846, $da60b8d0, $44042d73, $33031de5, $aa0a4c5f, $dd0d7cc9,
    $5005713c, $270241aa, $be0b1010, $c90c2086, $5768b525, $206f85b3, $b966d409, $ce61e49f,
    $5edef90e, $29d9c998, $b0d09822, $c7d7a8b4, $59b33d17, $2eb40d81, $b7bd5c3b, $c0ba6cad,
    $edb88320, $9abfb3b6, $03b6e20c, $74b1d29a, $ead54739, $9dd277af, $04db2615, $73dc1683,
    $e3630b12, $94643b84, $0d6d6a3e, $7a6a5aa8, $e40ecf0b, $9309ff9d, $0a00ae27, $7d079eb1,
    $f00f9344, $8708a3d2, $1e01f268, $6906c2fe, $f762575d, $806567cb, $196c3671, $6e6b06e7,
    $fed41b76, $89d32be0, $10da7a5a, $67dd4acc, $f9b9df6f, $8ebeeff9, $17b7be43, $60b08ed5,
    $d6d6a3e8, $a1d1937e, $38d8c2c4, $4fdff252, $d1bb67f1, $a6bc5767, $3fb506dd, $48b2364b,
    $d80d2bda, $af0a1b4c, $36034af6, $41047a60, $df60efc3, $a867df55, $316e8eef, $4669be79,
    $cb61b38c, $bc66831a, $256fd2a0, $5268e236, $cc0c7795, $bb0b4703, $220216b9, $5505262f,
    $c5ba3bbe, $b2bd0b28, $2bb45a92, $5cb36a04, $c2d7ffa7, $b5d0cf31, $2cd99e8b, $5bdeae1d,
    $9b64c2b0, $ec63f226, $756aa39c, $026d930a, $9c0906a9, $eb0e363f, $72076785, $05005713,
    $95bf4a82, $e2b87a14, $7bb12bae, $0cb61b38, $92d28e9b, $e5d5be0d, $7cdcefb7, $0bdbdf21,
    $86d3d2d4, $f1d4e242, $68ddb3f8, $1fda836e, $81be16cd, $f6b9265b, $6fb077e1, $18b74777,
    $88085ae6, $ff0f6a70, $66063bca, $11010b5c, $8f659eff, $f862ae69, $616bffd3, $166ccf45,
    $a00ae278, $d70dd2ee, $4e048354, $3903b3c2, $a7672661, $d06016f7, $4969474d, $3e6e77db,
    $aed16a4a, $d9d65adc, $40df0b66, $37d83bf0, $a9bcae53, $debb9ec5, $47b2cf7f, $30b5ffe9,
    $bdbdf21c, $cabac28a, $53b39330, $24b4a3a6, $bad03605, $cdd70693, $54de5729, $23d967bf,
    $b3667a2e, $c4614ab8, $5d681b02, $2a6f2b94, $b40bbe37, $c30c8ea1, $5a05df1b, $2d02ef8d
    );


const
  DefaultInMemSize = 256 * 1024; { Files larger than 256k are processed on disk   }
  DefaultBufSize = 16384;     { Use 16K file buffers                             }
  MINBITS = 9;        { Starting code size of 9 bits                     }
  MAXBITS = 13;        { Maximum code size of 13 bits                     }
  SPECIAL = 256;        { Special function code                            }
  INCSIZE = 1;        { Code indicating a jump in code size              }
  CLEARCODE = 2;        { Code indicating code table has been cleared      }
  STDATTR = faAnyFile;  { Standard file attribute for DOS Find First/Next  }

{ ---------------------------------------------------------------------
    Auxiliary
  ---------------------------------------------------------------------}
type
  // A local version of TFileStream which uses rawbytestring. It
  TFileStream = class(THandleStream)
  private
    FFileName: RawBytestring;
  public
    constructor Create(const AFileName: RawBytestring; Mode: word);
    constructor Create(const AFileName: RawBytestring; Mode: word; Rights: cardinal);
    destructor Destroy; override;
    property FileName: RawBytestring read FFilename;
  end;

constructor TFileStream.Create(const AFileName: rawbytestring; Mode: word);

begin
  Create(AFileName, Mode, 438);
end;


constructor TFileStream.Create(const AFileName: rawbytestring; Mode: word; Rights: cardinal);
var
  H: Thandle;
begin
  FFileName := AFileName;
  if (Mode and fmCreate) > 0 then
    H := FileCreate(AFileName, Mode, Rights)
  else
    H := FileOpen(AFileName, Mode);

  if (THandle(H) = feInvalidHandle) then
    if Mode = fmcreate then
      raise EFCreateError.createfmt(SFCreateError, [AFileName])
    else
      raise EFOpenError.Createfmt(SFOpenError, [AFilename]);
  inherited Create(H);
end;

destructor TFileStream.Destroy;
begin
  FileClose(Handle);
end;

{$IFDEF FPC_BIG_ENDIAN}
function SwapLFH(const Values: Local_File_Header_Type): Local_File_Header_Type;
begin
  with Values do
  begin
    Result.Signature := SwapEndian(Signature);
    Result.Extract_Version_Reqd := SwapEndian(Extract_Version_Reqd);
    Result.Bit_Flag := SwapEndian(Bit_Flag);
    Result.Compress_Method := SwapEndian(Compress_Method);
    Result.Last_Mod_Time := SwapEndian(Last_Mod_Time);
    Result.Last_Mod_Date := SwapEndian(Last_Mod_Date);
    Result.Crc32 := SwapEndian(Crc32);
    Result.Compressed_Size := SwapEndian(Compressed_Size);
    Result.Uncompressed_Size := SwapEndian(Uncompressed_Size);
    Result.Filename_Length := SwapEndian(Filename_Length);
    Result.Extra_Field_Length := SwapEndian(Extra_Field_Length);
  end;
end;

function SwapEDFH(const Values: Extensible_Data_Field_Header_Type): Extensible_Data_Field_Header_Type;
begin
  with Values do
  begin
    Result.Header_ID := SwapEndian(Header_ID);
    Result.Data_Size := SwapEndian(Data_Size);
  end;
end;

function SwapZ64EIF(const Values: Zip64_Extended_Info_Field_Type): Zip64_Extended_Info_Field_Type;
begin
  with Values do
  begin
    Result.Original_Size := SwapEndian(Original_Size);
    Result.Compressed_Size := SwapEndian(Compressed_Size);
    Result.Relative_Hdr_Offset := SwapEndian(Relative_Hdr_Offset);
    Result.Disk_Start_Number := SwapEndian(Disk_Start_Number);
  end;
end;

function SwapCFH(const Values: Central_File_Header_Type): Central_File_Header_Type;
begin
  with Values do
  begin
    Result.Signature := SwapEndian(Signature);
    Result.MadeBy_Version := SwapEndian(MadeBy_Version);
    Result.Extract_Version_Reqd := SwapEndian(Extract_Version_Reqd);
    Result.Bit_Flag := SwapEndian(Bit_Flag);
    Result.Compress_Method := SwapEndian(Compress_Method);
    Result.Last_Mod_Time := SwapEndian(Last_Mod_Time);
    Result.Last_Mod_Date := SwapEndian(Last_Mod_Date);
    Result.Crc32 := SwapEndian(Crc32);
    Result.Compressed_Size := SwapEndian(Compressed_Size);
    Result.Uncompressed_Size := SwapEndian(Uncompressed_Size);
    Result.Filename_Length := SwapEndian(Filename_Length);
    Result.Extra_Field_Length := SwapEndian(Extra_Field_Length);
    Result.File_Comment_Length := SwapEndian(File_Comment_Length);
    Result.Starting_Disk_Num := SwapEndian(Starting_Disk_Num);
    Result.Internal_Attributes := SwapEndian(Internal_Attributes);
    Result.External_Attributes := SwapEndian(External_Attributes);
    Result.Local_Header_Offset := SwapEndian(Local_Header_Offset);
  end;
end;

function SwapECD(const Values: End_of_Central_Dir_Type): End_of_Central_Dir_Type;
begin
  with Values do
  begin
    Result.Signature := SwapEndian(Signature);
    Result.Disk_Number := SwapEndian(Disk_Number);
    Result.Central_Dir_Start_Disk := SwapEndian(Central_Dir_Start_Disk);
    Result.Entries_This_Disk := SwapEndian(Entries_This_Disk);
    Result.Total_Entries := SwapEndian(Total_Entries);
    Result.Central_Dir_Size := SwapEndian(Central_Dir_Size);
    Result.Start_Disk_Offset := SwapEndian(Start_Disk_Offset);
    Result.ZipFile_Comment_Length := SwapEndian(ZipFile_Comment_Length);
  end;
end;

function SwapZ64ECD(const Values: Zip64_End_of_Central_Dir_Type): Zip64_End_of_Central_Dir_Type;
begin
  with Values do
  begin
    Result.Signature := SwapEndian(Signature);
    Result.Record_Size := SwapEndian(Record_Size);
    Result.Version_Made_By := SwapEndian(Version_Made_By);
    Result.Extract_Version_Reqd := SwapEndian(Extract_Version_Reqd);
    Result.Disk_Number := SwapEndian(Disk_Number);
    Result.Central_Dir_Start_Disk := SwapEndian(Central_Dir_Start_Disk);
    Result.Entries_This_Disk := SwapEndian(Entries_This_Disk);
    Result.Total_Entries := SwapEndian(Total_Entries);
    Result.Central_Dir_Size := SwapEndian(Central_Dir_Size);
    Result.Start_Disk_Offset := SwapEndian(Start_Disk_Offset);
  end;
end;

function SwapZ64ECDL(const Values: Zip64_End_of_Central_Dir_Locator_type): Zip64_End_of_Central_Dir_Locator_type;
begin
  with Values do
  begin
    Result.Signature := SwapEndian(Signature);
    Result.Zip64_EOCD_Start_Disk := SwapEndian(Zip64_EOCD_Start_Disk);
    Result.Central_Dir_Zip64_EOCD_Offset := SwapEndian(Central_Dir_Zip64_EOCD_Offset);
    Result.Total_Disks := SwapEndian(Total_Disks);
  end;
end;

{$ENDIF FPC_BIG_ENDIAN}

procedure DateTimeToZipDateTime(DT: TDateTime; out ZD, ZT: word);
var
  Y, M, D, H, N, S, MS: word;
begin
  DecodeDate(DT, Y, M, D);
  DecodeTime(DT, H, N, S, MS);
  if Y < 1980 then
  begin
    // Invalid date/time; set to earliest possible
    Y := 0;
    M := 1;
    D := 1;
    H := 0;
    N := 0;
    S := 0;
    MS := 0;
  end
  else
  begin
    Y := Y - 1980;
  end;
  ZD := d + (32 * M) + (512 * Y);
  ZT := (S div 2) + (32 * N) + (2048 * h);
end;

procedure ZipDateTimeToDateTime(ZD, ZT: word; out DT: TDateTime);
var
  Y, M, D, H, N, S, MS: word;
begin
  MS := 0;
  S := (ZT and 31) shl 1;
  N := (ZT shr 5) and 63;
  H := ZT shr 11;
  D := ZD and 31;
  M := (ZD shr 5) and 15;
  Y := ((ZD shr 9) and 127) + 1980;

  if M < 1 then
    M := 1;
  if D < 1 then
    D := 1;
  DT := ComposeDateTime(EncodeDate(Y, M, D), EncodeTime(H, N, S, MS));
end;

function ZipUnixAttrsToFatAttrs(const Name: string; Attrs: longint): longint;
begin
  Result := faArchive;

  if (Pos('.', Name) = 1) and (Name <> '.') and (Name <> '..') then
    Result := Result + faHidden;
  case (Attrs and UNIX_MASK) of
    UNIX_DIR: Result := Result + faDirectory;
    UNIX_LINK: Result := Result + faSymLink;
    UNIX_FIFO, UNIX_CHAR, UNIX_BLK, UNIX_SOCK:
      Result := Result + faSysFile;
  end;

  if (Attrs and UNIX_WUSR) = 0 then
    Result := Result + faReadOnly;
end;

function ZipFatAttrsToUnixAttrs(Attrs: longint): longint;
begin
  Result := UNIX_DEFAULT;
  if (faReadOnly and Attrs) > 0 then
    Result := Result and not (UNIX_WUSR);

  if (faSymLink and Attrs) > 0 then
    Result := Result or UNIX_LINK
  else
  if (faDirectory and Attrs) > 0 then
    Result := Result or UNIX_DIR
  else
    Result := Result or UNIX_FILE;
end;

function CRC32Str(const s: string): DWord;
var
  i: integer;
begin
  Result := $FFFFFFFF;
  if Length(S) > 0 then
    for i := 1 to Length(s) do
      Result := Crc_32_Tab[byte(Result xor longint(s[i]))] xor ((Result shr 8) and $00FFFFFF);
  Result := not Result;
end;

//================= TZipDeCompressor ==================================

procedure TZipDeCompressor.UpdC32(Octet: byte);
begin
  FCrc32Val := Crc_32_Tab[byte(FCrc32Val xor longint(Octet))] xor ((FCrc32Val shr 8) and $00FFFFFF);
end;

constructor TZipDeCompressor.Create(AInFile, AOutFile: TStream; ABufSize: longword);
begin
  FinFile := AInFile;
  FoutFile := AOutFile;
  FBufferSize := ABufSize;
  CRC32Val := $FFFFFFFF;
end;

procedure TZipDeCompressor.Terminate;
begin
  FTerminated := True;
end;

//================= TZipCompressor ===================================

procedure TZipCompressor.UpdC32(Octet: byte);
begin
  FCrc32Val := Crc_32_Tab[byte(FCrc32Val xor longint(Octet))] xor ((FCrc32Val shr 8) and $00FFFFFF);
end;

constructor TZipCompressor.Create(AInFile, AOutFile: TStream; ABufSize: longword);
begin
  FinFile := AInFile;
  FoutFile := AOutFile;
  FBufferSize := ABufSize;
  CRC32Val := $FFFFFFFF;
end;

procedure TZipCompressor.Terminate;
begin
  FTerminated := True;
end;

//================= TZipDeflater  =========================================

constructor TZipDeflater.Create(AInFile, AOutFile: TStream; ABufSize: longword);
begin
  inherited;
  FCompressionLevel := clDefault;
end;


procedure TZipDeflater.Compress;
var
  Buf: PByte;
  I, Count, NewCount: integer;
  C: TCompressionStream;
  BytesNow: int64;
  NextMark: int64;
  OnBytes: int64;
  FSize: int64;
begin
  CRC32Val := $FFFFFFFF;
  Buf := GetMem(FBufferSize);
  if FOnPercent = 0 then
    FOnPercent := 1;
  OnBytes := Round((FInFile.Size * FOnPercent) / 100);
  BytesNow := 0;
  NextMark := OnBytes;
  FSize := FInfile.Size;
  try
    C := TCompressionStream.Create(FCompressionLevel, FOutFile, True);
    try
      if assigned(FOnProgress) then
        fOnProgress(self, 0);
      repeat
        Count := FInFile.Read(Buf^, FBufferSize);
        for I := 0 to Count - 1 do
          UpdC32(Buf[i]);
        NewCount := Count;
        while (NewCount > 0) do
          NewCount := NewCount - C.Write(Buf^, NewCount);
        Inc(BytesNow, Count);
        if BytesNow > NextMark then
        begin
          if (FSize > 0) and assigned(FOnProgress) then
            FOnProgress(self, 100 * (BytesNow / FSize));
          Inc(NextMark, OnBytes);
        end;
      until (Count = 0) or Terminated;
    finally
      C.Free;
    end;
  finally
    FreeMem(Buf);
  end;
  if assigned(FOnProgress) then
    fOnProgress(self, 100.0);
  Crc32Val := not Crc32Val;
end;

class function TZipDeflater.ZipID: word;
begin
  Result := 8;
end;

class function TZipDeflater.ZipVersionReqd: word;
begin
  Result := 20;
end;

function TZipDeflater.ZipBitFlag: word;
begin
  case CompressionLevel of
    clnone: Result := %110;
    clfastest: Result := %100;
    cldefault: Result := %000;
    clmax: Result := %010;
    else
      Result := 0;
  end;
end;

//================= TZipInflater  ======================================

constructor TZipInflater.Create(AInFile, AOutFile: TStream; ABufSize: longword);
begin
  inherited;
end;

procedure TZipInflater.DeCompress;
var
  Buf: PByte;
  I, Count: integer;
  C: TDeCompressionStream;
  BytesNow: integer;
  NextMark: integer;
  OnBytes: integer;
  FSize: integer;
begin
  CRC32Val := $FFFFFFFF;
  if FOnPercent = 0 then
    FOnPercent := 1;
  OnBytes := Round((FInFile.Size * FOnPercent) / 100);
  BytesNow := 0;
  NextMark := OnBytes;
  FSize := FInfile.Size;

  if Assigned(FOnProgress) then
    fOnProgress(self, 0);

  Buf := GetMem(FBufferSize);
  try
    C := TDeCompressionStream.Create(FInFile, True);
    try
      repeat
        Count := C.Read(Buf^, FBufferSize);
        for I := 0 to Count - 1 do
          UpdC32(Buf[i]);
        FOutFile.Write(Buf^, Count);
        Inc(BytesNow, Count);
        if BytesNow > NextMark then
        begin
          if (FSize > 0) and assigned(FOnProgress) then
            FOnProgress(self, 100 * (BytesNow / FSize));
          if assigned(FOnProgressEx) then
            FOnProgressEx(Self, FTotPos + BytesNow, FTotSize);
          Inc(NextMark, OnBytes);
        end;
      until (Count = 0) or Terminated;
      FTotPos := FTotPos + FOutFile.Size;
    finally
      C.Free;
    end;
  finally
    FreeMem(Buf);
  end;
  if assigned(FOnProgress) then
    fOnProgress(self, 100.0);
  if assigned(FOnProgressEx) then
    FOnProgressEx(Self, FTotPos, FTotSize);
  Crc32Val := not Crc32Val;
end;

class function TZipInflater.ZipID: word;
begin
  Result := 8;
end;

//================= TZipShrinker ==========================================

constructor TZipShrinker.Create(AInFile, AOutFile: TStream; ABufSize: longword);
begin
  inherited;
  FBufSize := ABufSize;
  InBuf := GetMem(FBUFSIZE);
  OutBuf := GetMem(FBUFSIZE);
  CodeTable := GetMem(SizeOf(CodeTable^));
  FreeList := GetMem(SizeOf(FreeList^));
end;

destructor TZipShrinker.Destroy;
begin
  FreeMem(CodeTable);
  FreeMem(FreeList);
  FreeMem(InBuf);
  FreeMem(OutBuf);
  inherited Destroy;
end;

procedure TZipShrinker.Compress;
var
  OneString: string;
  Remaining: word;
begin
  BytesIn := 1;
  BytesOut := 1;
  InitializeCodeTable;
  FillInputBuffer;
  FirstCh := True;
  Crc32Val := $FFFFFFFF;
  FOnBytes := Round((FInFile.Size * FOnPercent) / 100);
  while not InputEof do
  begin
    Remaining := Succ(MaxInBufIdx - InBufIdx);
    if Remaining > 255 then
      Remaining := 255;
    if Remaining = 0 then
      FillInputBuffer
    else
    begin
      SetLength(OneString, Remaining);
      Move(InBuf[InBufIdx], OneString[1], Remaining);
      Inc(InBufIdx, Remaining);
      ProcessLine(OneString);
    end;
  end;
  Crc32Val := not Crc32Val;
  ProcessLine('');
end;

class function TZipShrinker.ZipID: word;
begin
  Result := 1;
end;

class function TZipShrinker.ZipVersionReqd: word;
begin
  Result := 10;
end;

function TZipShrinker.ZipBitFlag: word;
begin
  Result := 0;
end;


procedure TZipShrinker.DoOnProgress(const Pct: double);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Pct);
end;


procedure TZipShrinker.FillInputBuffer;
begin
  MaxInbufIDx := FInfile.Read(InBuf[0], FBufSize);
  if MaxInbufIDx = 0 then
    InputEof := True
  else
    InputEOF := False;
  InBufIdx := 0;
end;


procedure TZipShrinker.WriteOutputBuffer;
begin
  FOutFile.WriteBuffer(OutBuf[0], OutBufIdx);
  OutBufIdx := 0;
end;

procedure TZipShrinker.PutChar(B: byte);
begin
  OutBuf[OutBufIdx] := B;
  Inc(OutBufIdx);
  if OutBufIdx >= FBufSize then
    WriteOutputBuffer;
  Inc(BytesOut);
end;

procedure TZipShrinker.FlushOutput;
begin
  if OutBufIdx > 0 then
    WriteOutputBuffer;
end;

procedure TZipShrinker.PutCode(Code: smallint);
var
  ACode: longint;
  XSize: smallint;

begin
  if (Code = -1) then
  begin
    if BitsUsed > 0 then
      PutChar(SaveByte);
  end
  else
  begin
    ACode := longint(Code);
    XSize := CodeSize + BitsUsed;
    ACode := (ACode shl BitsUsed) or SaveByte;
    while (XSize div 8) > 0 do
    begin
      PutChar(Lo(ACode));
      ACode := ACode shr 8;
      Dec(XSize, 8);
    end;
    BitsUsed := XSize;
    SaveByte := Lo(ACode);
  end;
end;

procedure TZipShrinker.InitializeCodeTable;
var
  I: word;
begin
  for I := 0 to TableSize do
  begin
    with CodeTable^[I] do
    begin
      Child := -1;
      Sibling := -1;
      if (I <= 255) then
        Suffix := I;
    end;
    if (I >= 257) then
      FreeList^[I] := I;
  end;
  NextFree := FIRSTENTRY;
  TableFull := False;
end;


procedure TZipShrinker.Prune(Parent: word);
var
  CurrChild: smallint;
  NextSibling: smallint;
begin
  CurrChild := CodeTable^[Parent].Child;
  { Find first Child that has descendants .. clear any that don't }
  while (CurrChild <> -1) and (CodeTable^[CurrChild].Child = -1) do
  begin
    CodeTable^[Parent].Child := CodeTable^[CurrChild].Sibling;
    CodeTable^[CurrChild].Sibling := -1;
    { Turn on ClearList bit to indicate a cleared entry }
    ClearList[CurrChild div 8] := (ClearList[CurrChild div 8] or (1 shl (CurrChild mod 8)));
    CurrChild := CodeTable^[Parent].Child;
  end;
  if CurrChild <> -1 then
  begin   { If there are any children left ...}
    Prune(CurrChild);
    NextSibling := CodeTable^[CurrChild].Sibling;
    while NextSibling <> -1 do
    begin
      if CodeTable^[NextSibling].Child = -1 then
      begin
        CodeTable^[CurrChild].Sibling := CodeTable^[NextSibling].Sibling;
        CodeTable^[NextSibling].Sibling := -1;
        { Turn on ClearList bit to indicate a cleared entry }
        ClearList[NextSibling div 8] := (ClearList[NextSibling div 8] or (1 shl (NextSibling mod 8)));
        NextSibling := CodeTable^[CurrChild].Sibling;
      end
      else
      begin
        CurrChild := NextSibling;
        Prune(CurrChild);
        NextSibling := CodeTable^[CurrChild].Sibling;
      end;
    end;
  end;
end;


procedure TZipShrinker.Clear_Table;
var
  Node: word;
begin
  FillChar(ClearList, SizeOf(ClearList), $00);
  for Node := 0 to 255 do
    Prune(Node);
  NextFree := Succ(TABLESIZE);
  for Node := TABLESIZE downto FIRSTENTRY do
  begin
    if (ClearList[Node div 8] and (1 shl (Node mod 8))) <> 0 then
    begin
      Dec(NextFree);
      FreeList^[NextFree] := Node;
    end;
  end;
  if NextFree <= TABLESIZE then
    TableFull := False;
end;


procedure TZipShrinker.Table_Add(Prefix: word; Suffix: byte);
var
  FreeNode: word;
begin
  if NextFree <= TABLESIZE then
  begin
    FreeNode := FreeList^[NextFree];
    Inc(NextFree);
    CodeTable^[FreeNode].Child := -1;
    CodeTable^[FreeNode].Sibling := -1;
    CodeTable^[FreeNode].Suffix := Suffix;
    if CodeTable^[Prefix].Child = -1 then
      CodeTable^[Prefix].Child := FreeNode
    else
    begin
      Prefix := CodeTable^[Prefix].Child;
      while CodeTable^[Prefix].Sibling <> -1 do
        Prefix := CodeTable^[Prefix].Sibling;
      CodeTable^[Prefix].Sibling := FreeNode;
    end;
  end;
  if NextFree > TABLESIZE then
    TableFull := True;
end;

function TZipShrinker.Table_Lookup(TargetPrefix: smallint; TargetSuffix: byte;
  Out FoundAt: smallint): boolean;

var
  TempPrefix: smallint;

begin
  TempPrefix := TargetPrefix;
  Table_lookup := False;
  if CodeTable^[TempPrefix].Child <> -1 then
  begin
    TempPrefix := CodeTable^[TempPrefix].Child;
    repeat
      if CodeTable^[TempPrefix].Suffix = TargetSuffix then
      begin
        Table_lookup := True;
        break;
      end;
      if CodeTable^[TempPrefix].Sibling = -1 then
        break;
      TempPrefix := CodeTable^[TempPrefix].Sibling;
    until False;
  end;
  if Table_Lookup then
    FoundAt := TempPrefix
  else
    FoundAt := -1;
end;

procedure TZipShrinker.Shrink(Suffix: smallint);

const
  LastCode: smallint = 0;

var
  WhereFound: smallint;

begin
  if FirstCh then
  begin
    SaveByte := $00;
    BitsUsed := 0;
    CodeSize := MINBITS;
    MaxCode := (1 shl CodeSize) - 1;
    LastCode := Suffix;
    FirstCh := False;
  end
  else
  begin
    if Suffix <> -1 then
    begin
      if TableFull then
      begin
        Putcode(LastCode);
        PutCode(SPECIAL);
        Putcode(CLEARCODE);
        Clear_Table;
        Table_Add(LastCode, Suffix);
        LastCode := Suffix;
      end
      else
      begin
        if Table_Lookup(LastCode, Suffix, WhereFound) then
        begin
          LastCode := WhereFound;
        end
        else
        begin
          PutCode(LastCode);
          Table_Add(LastCode, Suffix);
          LastCode := Suffix;
          if (FreeList^[NextFree] > MaxCode) and (CodeSize < MaxBits) then
          begin
            PutCode(SPECIAL);
            PutCode(INCSIZE);
            Inc(CodeSize);
            MaxCode := (1 shl CodeSize) - 1;
          end;
        end;
      end;
    end
    else
    begin
      PutCode(LastCode);
      PutCode(-1);
      FlushOutput;
    end;
  end;
end;

procedure TZipShrinker.ProcessLine(const Source: string);

var
  I: word;

begin
  if Source = '' then
    Shrink(-1)
  else
    for I := 1 to Length(Source) do
    begin
      Inc(BytesIn);
      if (Pred(BytesIn) mod FOnBytes) = 0 then
        DoOnProgress(100 * (BytesIn / FInFile.Size));
      UpdC32(Ord(Source[I]));
      Shrink(Ord(Source[I]));
    end;
end;


//========== TplZipCompress ==============================

constructor TplZipCompress.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);

    FBufSize := DefaultBufSize;
    FInMemSize := DefaultInMemSize;
    FFiles := TStringList.Create;
    FEntries := TZipFileEntries.Create(TZipFileEntry);
    FOnPercent := 1;
    FZipFileNeedsZip64 := False;
    LocalZip64ExtHdr.Header_ID := ZIP64_HEADER_ID;
    LocalZip64ExtHdr.Data_Size := SizeOf(Zip64_Extended_Info_Field_Type);
  end;

destructor TplZipCompress.Destroy;
begin
  Clear;
  FreeAndNil(FEntries);
  FreeAndNil(FFiles);
  inherited;
end;

procedure TplZipCompress.GetFileInfo;

var
  F: TZipFileEntry;
  Info: TSearchRec;
  I: integer; //zip spec allows QWord but FEntries.Count does not support it
{$IFDEF UNIX}
  UnixInfo: Stat;
{$ENDIF}
begin
  for I := 0 to FEntries.Count - 1 do
  begin
    F := FEntries[i];
    if F.Stream = nil then
    begin
      if (F.DiskFileName = '') then
        raise EZipError.CreateFmt(SErrMissingFileName, [I]);
      if FindFirst(F.DiskFileName, STDATTR, Info) = 0 then
        try
          F.Size := Info.Size;
          F.DateTime := FileDateToDateTime(Info.Time);
        {$IFDEF UNIX}
          if fplstat(F.DiskFileName, @UnixInfo) = 0 then
            F.Attributes := UnixInfo.st_mode;
        {$ELSE}
          F.Attributes := Info.Attr;
        {$ENDIF}
        finally
          FindClose(Info);
        end
      else
        raise EZipError.CreateFmt(SErrFileDoesNotExist, [F.DiskFileName]);
    end
    else
    begin
      if (F.ArchiveFileName = '') then
        raise EZipError.CreateFmt(SErrMissingArchiveName, [I]);
      F.Size := F.Stream.Size;
      if (F.Attributes = 0) then
      begin
      {$IFDEF UNIX}
        F.Attributes := UNIX_FILE or UNIX_DEFAULT;
      {$ELSE}
        F.Attributes := faArchive;
      {$ENDIF}
      end;
    end;
  end;
end;


procedure TplZipCompress.SetEntries(const AValue: TZipFileEntries);
begin
  if FEntries = AValue then
    exit;
  FEntries.Assign(AValue);
end;

function TplZipCompress.OpenInput(Item: TZipFileEntry): boolean;

begin
  if (Item.Stream <> nil) then
    FInFile := Item.Stream
  else
  if Item.IsDirectory then
    FInFile := TStringStream.Create('')
  else
    FInFile := TFileStream.Create(Item.DiskFileName, fmOpenRead);
  Result := True;
  if Assigned(FOnStartFile) then
    FOnStartFile(Self, Item.ArchiveFileName);
end;


procedure TplZipCompress.CloseInput(Item: TZipFileEntry);

begin
  if (FInFile <> Item.Stream) then
    FreeAndNil(FInFile)
  else
    FinFile := nil;
  DoEndOfFile;
end;


procedure TplZipCompress.StartZipFile(Item: TZipFileEntry);

begin
  FillChar(LocalHdr, SizeOf(LocalHdr), 0);
  FillChar(LocalZip64Fld, SizeOf(LocalZip64Fld), 0);
  with LocalHdr do
  begin
    Signature := LOCAL_FILE_HEADER_SIGNATURE;
    Extract_Version_Reqd := 20; //default value, v2.0
    Bit_Flag := 0;
    Compress_Method := 1;
    DateTimeToZipDateTime(Item.DateTime, Last_Mod_Date, Last_Mod_Time);
    Crc32 := 0;
    Compressed_Size := 0;
    LocalZip64Fld.Compressed_Size := 0;
    if Item.Size >= $FFFFFFFF then
    begin
      Uncompressed_Size := $FFFFFFFF;
      LocalZip64Fld.Original_Size := Item.Size;
    end
    else
    begin
      Uncompressed_Size := Item.Size;
      LocalZip64Fld.Original_Size := 0;
    end;
    FileName_Length := 0;
    if (LocalZip64Fld.Original_Size > 0) or (LocalZip64Fld.Compressed_Size > 0) or (LocalZip64Fld.Disk_Start_Number > 0) or
      (LocalZip64Fld.Relative_Hdr_Offset > 0) then
      Extra_Field_Length := SizeOf(LocalZip64ExtHdr) + SizeOf(LocalZip64Fld)
    else
      Extra_Field_Length := 0;
  end;
end;


function TplZipCompress.UpdateZipHeader(Item: TZipFileEntry; FZip: TStream; ACRC: longword; AMethod: word; AZipVersionReqd: word;
  AZipBitFlag: word): boolean;
  // Update header for a single zip file (local header)
var
  IsZip64: boolean; //Must the local header be in zip64 format?
  // Separate from zip64 status of entire zip file.
  ZFileName: string;
begin
  ZFileName := Item.ArchiveFileName;
  IsZip64 := False;
  with LocalHdr do
  begin
    FileName_Length := Length(ZFileName);
    Crc32 := ACRC;
    if LocalZip64Fld.Original_Size > 0 then
      Result := not (FZip.Size >= LocalZip64Fld.Original_Size)
    else
      Result := not (Compressed_Size >= Uncompressed_Size);
    if Item.CompressionLevel = clNone then
      Result := False; //user wishes override or invalid compression
    if not Result then
    begin
      Compress_Method := 0; // No use for compression: change storage type & compression size...
      if LocalZip64Fld.Original_Size > 0 then
      begin
        IsZip64 := True;
        Compressed_Size := $FFFFFFFF;
        LocalZip64Fld.Compressed_Size := LocalZip64Fld.Original_Size;
      end
      else
      begin
        Compressed_Size := Uncompressed_Size;
        LocalZip64Fld.Compressed_Size := 0;
      end;
    end
    else { Using compression }
    begin
      Compress_method := AMethod;
      Bit_Flag := Bit_Flag or AZipBitFlag;
      if FZip.Size >= $FFFFFFFF then
      begin
        IsZip64 := True;
        Compressed_Size := $FFFFFFFF;
        LocalZip64Fld.Compressed_Size := FZip.Size;
      end
      else
      begin
        Compressed_Size := FZip.Size;
        LocalZip64Fld.Compressed_Size := 0;
      end;
      if AZipVersionReqd > Extract_Version_Reqd then
        Extract_Version_Reqd := AZipVersionReqd;
    end;
    if (IsZip64) and (Extract_Version_Reqd < 45) then
      Extract_Version_Reqd := 45;
  end;
  if IsZip64 then
    LocalHdr.Extra_Field_Length := SizeOf(LocalZip64ExtHdr) + SizeOf(LocalZip64Fld);
  FOutStream.WriteBuffer(
{$IFDEF ENDIAN_BIG}
    SwapLFH
{$ENDIF}
    (LocalHdr), SizeOf(LocalHdr));
  // Append extensible field header+zip64 extensible field if needed:
  FOutStream.WriteBuffer(ZFileName[1], Length(ZFileName));
  if IsZip64 then
  begin
    LocalZip64ExtHdr.Header_ID := ZIP64_HEADER_ID;
    FOutStream.WriteBuffer(
{$IFDEF ENDIAN_BIG}
      SwapEDFH
{$ENDIF}
      (LocalZip64ExtHdr), SizeOf(LocalZip64ExtHdr));
    FOutStream.WriteBuffer(
{$IFDEF ENDIAN_BIG}
      SwapZ64EIF
{$ENDIF}
      (LocalZip64Fld), SizeOf(LocalZip64Fld));
  end;
end;


procedure TplZipCompress.BuildZipDirectory;
// Write out all central file headers using info from local headers
var
  SavePos: int64;
  HdrPos: int64; //offset from disk where file begins to local header
  CenDirPos: int64;
  ACount: QWord; //entry counter
  ZFileName: string; //archive filename
  IsZip64: boolean; //local header=zip64 format?
  MinReqdVersion: word; //minimum needed to extract
  ExtInfoHeader: Extensible_Data_Field_Header_Type;
  Zip64ECD: Zip64_End_of_Central_Dir_type;
  Zip64ECDL: Zip64_End_of_Central_Dir_Locator_type;
begin
  ACount := 0;
  MinReqdVersion := 0;
  CenDirPos := FOutStream.Position;
  FOutStream.Seek(0, soBeginning);             { Rewind output file }
  HdrPos := FOutStream.Position;
  FOutStream.ReadBuffer(LocalHdr, SizeOf(LocalHdr));
{$IFDEF FPC_BIG_ENDIAN}
  LocalHdr := SwapLFH(LocalHdr);
{$ENDIF}
  repeat
    SetLength(ZFileName, LocalHdr.FileName_Length);
    FOutStream.ReadBuffer(ZFileName[1], LocalHdr.FileName_Length);
    IsZip64 := (LocalHdr.Compressed_Size = $FFFFFFFF) or (LocalHdr.Uncompressed_Size = $FFFFFFFF) or (HdrPos >= $FFFFFFFF);
    FillChar(LocalZip64Fld, SizeOf(LocalZip64Fld), 0); // easier to check compressed length
    if LocalHdr.Extra_Field_Length > 0 then
    begin
      SavePos := FOutStream.Position;
      if (IsZip64 and (LocalHdr.Extra_Field_Length >= SizeOf(LocalZip64ExtHdr) + SizeOf(LocalZip64Fld))) then
        while FOutStream.Position < SavePos + LocalHdr.Extra_Field_Length do
        begin
          FOutStream.ReadBuffer(ExtInfoHeader, SizeOf(ExtInfoHeader));
        {$IFDEF FPC_BIG_ENDIAN}
          ExtInfoHeader := SwapEDFH(ExtInfoHeader);
        {$ENDIF}
          if ExtInfoHeader.Header_ID = ZIP64_HEADER_ID then
          begin
            FOutStream.ReadBuffer(LocalZip64Fld, SizeOf(LocalZip64Fld));
          {$IFDEF FPC_BIG_ENDIAN}
            LocalZip64Fld := SwapZ64EIF(LocalZip64Fld);
          {$ENDIF}
          end
          else
          begin
            // Read past non-zip64 extra field
            FOutStream.Seek(ExtInfoHeader.Data_Size, soFromCurrent);
          end;
        end;
      // Move past extra fields
      FOutStream.Seek(SavePos + LocalHdr.Extra_Field_Length, soFromBeginning);
    end;
    SavePos := FOutStream.Position;
    FillChar(CentralHdr, SizeOf(CentralHdr), 0);
    with CentralHdr do
    begin
      Signature := CENTRAL_FILE_HEADER_SIGNATURE;
      MadeBy_Version := LocalHdr.Extract_Version_Reqd;
      if (IsZip64) and (MadeBy_Version < 45) then
        MadeBy_Version := 45;
    {$IFDEF UNIX}
      {$IFDEF DARWIN}//OSX
      MadeBy_Version := MadeBy_Version or (OS_OSX shl 8);
      {$ELSE}
      MadeBy_Version := MadeBy_Version or (OS_UNIX shl 8);
      {$ENDIF}
    {$ENDIF}
    {$IFDEF OS2}
      MadeBy_Version := MadeBy_Version or (OS_OS2 shl 8);
    {$ENDIF}
      {$warning TODO: find a way to recognize VFAT and NTFS}
      // Copy over extract_version_reqd..extra_field_length
      Move(LocalHdr.Extract_Version_Reqd, Extract_Version_Reqd, 26);
      if (IsZip64) and (Extract_Version_Reqd < 45) then
        Extract_Version_Reqd := 45;
      // Keep track of the minimum version required to extract
      // zip file as a whole
      if Extract_Version_Reqd > MinReqdVersion then
        MinReqdVersion := Extract_Version_Reqd;
      Last_Mod_Time := localHdr.Last_Mod_Time;
      Last_Mod_Date := localHdr.Last_Mod_Date;
      File_Comment_Length := 0;
      Starting_Disk_Num := 0;
      Internal_Attributes := 0;
    {$IFDEF UNIX}
      External_Attributes := Entries[ACount].Attributes shl 16;
    {$ELSE}
      External_Attributes := Entries[ACount].Attributes;
    {$ENDIF}
      if HdrPos >= $FFFFFFFF then
      begin
        FZipFileNeedsZip64 := True;
        IsZip64 := True;
        Local_Header_offset := $FFFFFFFF;
        // LocalZip64Fld will be written out as central dir extra field later
        LocalZip64Fld.Relative_Hdr_Offset := HdrPos;
      end
      else
        Local_Header_Offset := HdrPos;
    end;
    FOutStream.Seek(0, soEnd);
    FOutStream.WriteBuffer(
{$IFDEF FPC_BIG_ENDIAN}
      SwapCFH
{$ENDIF}
      (CentralHdr), SizeOf(CentralHdr));
    FOutStream.WriteBuffer(ZFileName[1], Length(ZFileName));
    if IsZip64 then
    begin
      FOutStream.Seek(0, soEnd);
      FOutStream.WriteBuffer(
{$IFDEF FPC_BIG_ENDIAN}
        SwapEDFH
{$ENDIF}
        (LocalZip64ExtHdr), SizeOf(LocalZip64ExtHdr));
      FOutStream.WriteBuffer(
{$IFDEF FPC_BIG_ENDIAN}
        SwapZ64EIF
{$ENDIF}
        (LocalZip64Fld), SizeOf(LocalZip64Fld));
    end;

    Inc(ACount);
    // Move past compressed file data to next header:
    if Iszip64 then
      FOutStream.Seek(SavePos + LocalZip64Fld.Compressed_Size, soBeginning)
    else
      FOutStream.Seek(SavePos + LocalHdr.Compressed_Size, soBeginning);
    HdrPos := FOutStream.Position;
    FOutStream.ReadBuffer(LocalHdr, SizeOf(LocalHdr));
  {$IFDEF FPC_BIG_ENDIAN}
    LocalHdr := SwapLFH(LocalHdr);
  {$ENDIF}
  until LocalHdr.Signature = CENTRAL_FILE_HEADER_SIGNATURE;

  FOutStream.Seek(0, soEnd);
  FillChar(EndHdr, SizeOf(EndHdr), 0);

  // Write end of central directory record
  // We'll use the zip64 variants to store counts etc
  // and copy to the old record variables if possible
  // This seems to match expected behaviour of unzippers like
  // unrar that only look at the zip64 record
  FillChar(Zip64ECD, SizeOf(Zip64ECD), 0);
  Zip64ECD.Signature := ZIP64_END_OF_CENTRAL_DIR_SIGNATURE;
  FillChar(Zip64ECDL, SizeOf(Zip64ECDL), 0);
  Zip64ECDL.Signature := ZIP64_END_OF_CENTRAL_DIR_LOCATOR_SIGNATURE;
  Zip64ECDL.Total_Disks := 1; //default and no support for multi disks yet anyway
  with EndHdr do
  begin
    Signature := END_OF_CENTRAL_DIR_SIGNATURE;
    Disk_Number := 0;
    Central_Dir_Start_Disk := 0;

    Zip64ECD.Entries_This_Disk := ACount;
    Zip64ECD.Total_Entries := Acount;
    if ACount > $FFFF then
    begin
      FZipFileNeedsZip64 := True;
      Entries_This_Disk := $FFFF;
      Total_Entries := $FFFF;
    end
    else
    begin
      Entries_This_Disk := Zip64ECD.Entries_This_Disk;
      Total_Entries := Zip64ECD.Total_Entries;
    end;

    Zip64ECD.Central_Dir_Size := FOutStream.Size - CenDirPos;
    if (Zip64ECD.Central_Dir_Size) > $FFFFFFFF then
    begin
      FZipFileNeedsZip64 := True;
      Central_Dir_Size := $FFFFFFFF;
    end
    else
    begin
      Central_Dir_Size := Zip64ECD.Central_Dir_Size;
    end;

    Zip64ECD.Start_Disk_Offset := CenDirPos;
    if Zip64ECD.Start_Disk_Offset > $FFFFFFFF then
    begin
      FZipFileNeedsZip64 := True;
      Start_Disk_Offset := $FFFFFFFF;
    end
    else
    begin
      Start_Disk_Offset := Zip64ECD.Start_Disk_Offset;
    end;

    ZipFile_Comment_Length := Length(FFileComment);

    if FZipFileNeedsZip64 then
    begin
      //Write zip64 end of central directory record if needed
      if MinReqdVersion < 45 then
        MinReqdVersion := 45;
      Zip64ECD.Extract_Version_Reqd := MinReqdVersion;
      Zip64ECD.Version_Made_By := MinReqdVersion;
      Zip64ECD.Record_Size := SizeOf(Zip64ECD) - 12; //Assumes no variable length field following
      Zip64ECDL.Central_Dir_Zip64_EOCD_Offset := FOutStream.Position;
      Zip64ECDL.Zip64_EOCD_Start_Disk := 0;
      FOutStream.WriteBuffer(
{$IFDEF FPC_BIG_ENDIAN}
        SwapZ64ECD
{$ENDIF}
        (Zip64ECD), SizeOf(Zip64ECD));

      //Write zip64 end of central directory locator if needed
      FOutStream.WriteBuffer(
{$IFDEF FPC_BIG_ENDIAN}
        SwapZ64ECDL
{$ENDIF}
        (Zip64ECDL), SizeOf(Zip64ECDL));
    end;

    FOutStream.WriteBuffer(
{$IFDEF FPC_BIG_ENDIAN}
      SwapECD
{$ENDIF}
      (EndHdr), SizeOf(EndHdr));
    if Length(FFileComment) > 0 then
      FOutStream.WriteBuffer(FFileComment[1], Length(FFileComment));
  end;
end;

function TplZipCompress.CreateCompressor(Item: TZipFileEntry; AInFile, AZipStream: TStream): TZipCompressor;
begin
  Result := TZipDeflater.Create(AinFile, AZipStream, FBufSize);
  (Result as TZipDeflater).CompressionLevel := Item.CompressionLevel;
  FCurrenZipCompressor := Result;
end;

procedure TplZipCompress.ZipOneFile(Item: TZipFileEntry);
var
  CRC: longword;
  ZMethod: word;
  ZVersionReqd: word;
  ZBitFlag: word;
  ZipStream: TStream;
  TmpFileName: string;

begin
  OpenInput(Item);
  try
    StartZipFile(Item);
    if (FInfile.Size <= FInMemSize) then
      ZipStream := TMemoryStream.Create
    else
    begin
      TmpFileName := ChangeFileExt(FFileName, '.tmp');
      if TmpFileName = FFileName then
        TmpFileName := TmpFileName + '.tmp';
      ZipStream := TFileStream.Create(TmpFileName, fmCreate);
    end;
    try
      with CreateCompressor(Item, FinFile, ZipStream) do
        try
          OnProgress := Self.OnProgress;
          OnPercent := Self.OnPercent;
          Compress;
          CRC := Crc32Val;
          ZMethod := ZipID;
          ZVersionReqd := ZipVersionReqd;
          ZBitFlag := ZipBitFlag;
        finally
          FCurrenZipCompressor := nil;
          Free;
        end;
      if UpdateZipHeader(Item, ZipStream, CRC, ZMethod, ZVersionReqd, ZBitFlag) then
        // Compressed file smaller than original file.
        FOutStream.CopyFrom(ZipStream, 0)
      else
      begin
        // Original file smaller than compressed file.
        FInfile.Seek(0, soBeginning);
        FOutStream.CopyFrom(FInFile, 0);
      end;
    finally
      ZipStream.Free;
      if (TmpFileName <> '') then
        DeleteFile(TmpFileName);
    end;
  finally
    CloseInput(Item);
  end;
end;

// Just like SaveToFile, but uses the FileName property
procedure TplZipCompress.ZipAllFiles;
begin
  SaveToFile(FileName);
end;

procedure TplZipCompress.SaveToFile(AFileName: RawByteString);
var
  lStream: TFileStream;
begin
  FFileName := AFileName;
  lStream := TFileStream.Create(FFileName, fmCreate);
  try
    SaveToStream(lStream);
  finally
    FreeAndNil(lStream);
  end;
end;

procedure TplZipCompress.SaveToStream(AStream: TStream);
var
  I: integer; //could be qword but limited by FEntries.Count
begin
  FTerminated := False;
  FOutStream := AStream;
  if CheckEntries = 0 then
    Exit;
  FZipping := True;
  try
    GetFileInfo; //get info on file entries in zip
    I := 0;
    while (I < FEntries.Count) and not Terminated do
    begin
      ZipOneFile(FEntries[i]);
      Inc(I);
    end;
    if (FEntries.Count > 0) and not Terminated then
      BuildZipDirectory;
  finally
    FZipping := False;
    // Remove entries that have been added by CheckEntries from Files.
    for I := 0 to FFiles.Count - 1 do
      FEntries.Delete(FEntries.Count - 1);
  end;
end;

procedure TplZipCompress.SetBufSize(Value: longword);
begin
  if FZipping then
    raise EZipError.Create(SErrBufsizeChange);
  if Value >= DefaultBufSize then
    FBufSize := Value;
end;

procedure TplZipCompress.SetFileName(Value: RawByteString);
begin
  if FZipping then
    raise EZipError.Create(SErrFileChange);
  FFileName := Value;
end;

procedure TplZipCompress.ZipFiles(AFileName: RawByteString; FileList: TStrings);
begin
  FFileName := AFileName;
  ZipFiles(FileList);
end;

procedure TplZipCompress.ZipFiles(FileList: TStrings);
begin
  FFiles.Assign(FileList);
  ZipAllFiles;
end;

procedure TplZipCompress.ZipFiles(AFileName: RawByteString; Entries: TZipFileEntries);
begin
  FFileName := AFileName;
  ZipFiles(Entries);
end;

procedure TplZipCompress.ZipFiles(Entries: TZipFileEntries);
begin
  FEntries.Assign(Entries);
  ZipAllFiles;
end;

procedure TplZipCompress.DoEndOfFile;
var
  ComprPct: double;
begin
  if (FZipFileNeedsZip64) and (LocalZip64Fld.Original_Size > 0) then
    ComprPct := (100.0 * (LocalZip64Fld.Original_size - LocalZip64Fld.Compressed_Size)) / LocalZip64Fld.Original_Size
  else if (LocalHdr.Uncompressed_Size > 0) then
    ComprPct := (100.0 * (LocalHdr.Uncompressed_Size - LocalHdr.Compressed_Size)) / LocalHdr.Uncompressed_Size
  else
    ComprPct := 0;
  if Assigned(FOnEndOfFile) then
    FOnEndOfFile(Self, ComprPct);
end;

function TplZipCompress.CheckEntries: integer;
var
  I: integer; //Could be QWord but limited by FFiles.Count
begin
  for I := 0 to FFiles.Count - 1 do
    FEntries.AddFileEntry(FFiles[i]);

  // Use zip64 when number of file entries
  // or individual (un)compressed sizes
  // require it.
  if FEntries.Count >= $FFFF then
    FZipFileNeedsZip64 := True;

  if not (FZipFileNeedsZip64) then
  begin
    for I := 0 to FFiles.Count - 1 do
    begin
      if FEntries[i].FNeedsZip64 then
      begin
        FZipFileNeedsZip64 := True;
        break;
      end;
    end;
  end;

  Result := FEntries.Count;
end;

procedure TplZipCompress.Clear;
begin
  FEntries.Clear;
  FFiles.Clear;
end;

procedure TplZipCompress.Terminate;
begin
  FTerminated := True;
  if Assigned(FCurrenZipCompressor) then
    FCurrenZipCompressor.Terminate;
end;

//================ TplZipUnCompress ===================================

constructor TplZipUnCompress.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBufSize := DefaultBufSize;
  FFiles := TStringList.Create;
  TStringList(FFiles).Sorted := True;
  FEntries := TFullZipFileEntries.Create(TFullZipFileEntry);
  FOnPercent := 1;
end;


destructor TplZipUnCompress.Destroy;
begin
  Clear;
  FreeAndNil(FFiles);
  FreeAndNil(FEntries);
  inherited;
end;

procedure TplZipUnCompress.OpenInput;
begin
  if Assigned(FOnOpenInputStream) then
    FOnOpenInputStream(Self, FZipStream);
  if FZipStream = nil then
    FZipStream := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);
end;


function TplZipUnCompress.OpenOutput(OutFileName: RawByteString; out OutStream: TStream; Item: TFullZipFileEntry): boolean;
var
  Path: RawByteString;
  OldDirectorySeparators: set of char;
begin
  { the default RTL behavior is broken on Unix platforms
    for Windows compatibility: it allows both '/' and '\'
    as directory separator. We don't want that behavior
    here, since 'abc\' is a valid file name under Unix.

    The zip standard appnote.txt says zip files must have '/' as path
    separator, even on Windows: 4.4.17.1:
    "The path stored MUST not contain a drive or device letter, or a leading
    slash. All slashes MUST be forward slashes '/' as opposed to backwards
    slashes '\'" See also mantis issue #15836
    However, old versions of FPC on Windows (and possibly other utilities)
    created incorrect zip files with \ separator, so accept these as well as
    they're not valid in Windows file names anyway.
  }
  OldDirectorySeparators := AllowDirectorySeparators;
  {$ifdef Windows}
  // Explicitly allow / and \ regardless of what Windows supports
  AllowDirectorySeparators := ['\', '/'];
  {$else}
  // Follow the standard: only allow / regardless of actual separator on OS
  AllowDirectorySeparators := ['/'];
  {$endif}
  Path := ExtractFilePath(OutFileName);
  OutStream := nil;
  if Assigned(FOnCreateStream) then
    FOnCreateStream(Self, OutStream, Item);
  // If FOnCreateStream didn't create one, we create one now.
  if (OutStream = nil) then
  begin
    if (Path <> '') then
      ForceDirectories(Path);
    AllowDirectorySeparators := OldDirectorySeparators;
    OutStream := TFileStream.Create(OutFileName, fmCreate);

  end;

  AllowDirectorySeparators := OldDirectorySeparators;
  Result := True;
  if Assigned(FOnStartFile) then
    FOnStartFile(Self, OutFileName);
end;


procedure TplZipUnCompress.CloseOutput(Item: TFullZipFileEntry; var OutStream: TStream);
begin
  if Assigned(FOnDoneStream) then
  begin
    FOnDoneStream(Self, OutStream, Item);
    OutStream := nil;
  end
  else
    FreeAndNil(OutStream);
  DoEndOfFile;
end;


procedure TplZipUnCompress.CloseInput;
begin
  if Assigned(FOnCloseInputStream) then
    FOnCloseInputStream(Self, FZipStream);
  FreeAndNil(FZipStream);
end;


procedure TplZipUnCompress.ReadZipHeader(Item: TFullZipFileEntry; out AMethod: word);
var
  S: string;
  U: UTF8String;
  D: TDateTime;
  ExtraFieldHdr: Extensible_Data_Field_Header_Type;
  SavePos: int64; //could be qword but limited by stream
  // Infozip unicode path
  Infozip_Unicode_Path_Ver: byte;
  Infozip_Unicode_Path_CRC32: DWord;
begin
  FZipStream.Seek(Item.HdrPos, soBeginning);
  FZipStream.ReadBuffer(LocalHdr, SizeOf(LocalHdr));
{$IFDEF FPC_BIG_ENDIAN}
  LocalHdr := SwapLFH(LocalHdr);
{$ENDIF}
  FillChar(LocalZip64Fld, SizeOf(LocalZip64Fld), 0); //ensure no erroneous info
  with LocalHdr do
  begin
    Item.FBitFlags := Bit_Flag;
    SetLength(S, Filename_Length);
    FZipStream.ReadBuffer(S[1], Filename_Length);
    Item.ArchiveFileName := S;
    Item.DiskFileName := S;
    SavePos := FZipStream.Position; //after filename, before extra fields
    if Extra_Field_Length > 0 then
    begin
      SavePos := FZipStream.Position;
      if (LocalHdr.Extra_Field_Length >= SizeOf(ExtraFieldHdr)) then
        while FZipStream.Position < SavePos + LocalHdr.Extra_Field_Length do
        begin
          FZipStream.ReadBuffer(ExtraFieldHdr, SizeOf(ExtraFieldHdr));
          {$IFDEF FPC_BIG_ENDIAN}
          ExtraFieldHdr := SwapEDFH(ExtraFieldHdr);
          {$ENDIF}
          if ExtraFieldHdr.Header_ID = ZIP64_HEADER_ID then
          begin
            FZipStream.ReadBuffer(LocalZip64Fld, SizeOf(LocalZip64Fld));
            {$IFDEF FPC_BIG_ENDIAN}
            LocalZip64Fld := SwapZ64EIF(LocalZip64Fld);
            {$ENDIF}
          end
          // Infozip unicode path
          else if ExtraFieldHdr.Header_ID = INFOZIP_UNICODE_PATH_ID then
          begin
            FZipStream.ReadBuffer(Infozip_Unicode_Path_Ver, 1);
            if Infozip_Unicode_Path_Ver = 1 then
            begin
              FZipStream.ReadBuffer(Infozip_Unicode_Path_CRC32, sizeof(Infozip_Unicode_Path_CRC32));
                {$IFDEF FPC_BIG_ENDIAN}
              Infozip_Unicode_Path_CRC32 := SwapEndian(Infozip_Unicode_Path_CRC32);
                {$ENDIF}
              if CRC32Str(S) = Infozip_Unicode_Path_CRC32 then
              begin
                SetLength(U, ExtraFieldHdr.Data_Size - 5);
                FZipStream.ReadBuffer(U[1], Length(U));
                Item.UTF8ArchiveFileName := U;
                Item.UTF8DiskFileName := U;
              end
              else
                FZipStream.Seek(ExtraFieldHdr.Data_Size - 5, soFromCurrent);
            end
            else
              FZipStream.Seek(ExtraFieldHdr.Data_Size - 1, soFromCurrent);
          end
          else
            FZipStream.Seek(ExtraFieldHdr.Data_Size, soFromCurrent);
        end;
      // Move past extra fields
      FZipStream.Seek(SavePos + Extra_Field_Length, soFromBeginning);
    end;
    Item.Size := Uncompressed_Size;
    ZipDateTimeToDateTime(Last_Mod_Date, Last_Mod_Time, D);
    Item.DateTime := D;
    if Crc32 <> 0 then
      Item.CRC32 := Crc32;
    AMethod := Compress_method;
  end;
end;

procedure TplZipUnCompress.FindEndHeaders(out AEndHdr: End_of_Central_Dir_Type; out AEndHdrPos: int64;
  out AEndZip64Hdr: Zip64_End_of_Central_Dir_type; out AEndZip64HdrPos: int64);
// Reads backwords from the end of the zip file,
// following end of central directory, and, if present
// zip64 end of central directory locator and
// zip64 end of central directory record

// If valid regular end of directory found, AEndHdrPos>0
// If valid zip64 end of directory found, AEndZip64HdrPos>0
var
  EndZip64Locator: Zip64_End_of_Central_Dir_Locator_type;

  procedure SearchForSignature;
  // Search for end of central directory record signature
  // If failed, set AEndHdrPos to 0
  var
    I: integer;
    Buf: PByte;
    BufSize: integer;
    Result: boolean;
  begin
    Result := False;
    // scan the last (64k + something) bytes for the END_OF_CENTRAL_DIR_SIGNATURE
    // (zip file comments are 64k max).
    BufSize := 65536 + SizeOf(AEndHdr) + 128;
    if FZipStream.Size < BufSize then
      BufSize := FZipStream.Size;

    Buf := GetMem(BufSize);
    try
      FZipStream.Seek(FZipStream.Size - BufSize, soBeginning);
      FZipStream.ReadBuffer(Buf^, BufSize);

      for I := BufSize - SizeOf(AEndHdr) downto 0 do
      begin
        if (Buf[I] or (Buf[I + 1] shl 8) or (Buf[I + 2] shl 16) or (Buf[I + 3] shl 24)) = END_OF_CENTRAL_DIR_SIGNATURE then
        begin
          Move(Buf[I], AEndHdr, SizeOf(AEndHdr));
          {$IFDEF FPC_BIG_ENDIAN}
          AEndHdr := SwapECD(AEndHdr);
          {$ENDIF}
          if (AEndHdr.Signature = END_OF_CENTRAL_DIR_SIGNATURE) and (I + SizeOf(AEndHdr) + AEndHdr.ZipFile_Comment_Length = BufSize) then
          begin
            AEndHdrPos := FZipStream.Size - BufSize + I;
            FZipStream.Seek(AEndHdrPos + SizeOf(AEndHdr), soBeginning);
            SetLength(FFileComment, AEndHdr.ZipFile_Comment_Length);
            FZipStream.ReadBuffer(FFileComment[1], Length(FFileComment));
            Result := True; //found it
            break;
          end;
        end;
      end;
    finally
      FreeMem(Buf);
    end;
    if not (Result) then
    begin
      AEndHdrPos := 0;
      FillChar(AEndHdr, SizeOf(AEndHdr), 0);
    end;
  end;

  procedure ZeroData;
  begin
    AEndHdrPos := 0;
    FillChar(AEndHdr, SizeOf(AEndHdr), 0);
    AEndZip64HdrPos := 0;
    FillChar(AEndZip64Hdr, SizeOf(AEndZip64Hdr), 0);
  end;

begin
  // Zip64 records may not exist, so fill out default values
  FillChar(AEndZip64Hdr, SizeOf(AEndZip64Hdr), 0);
  AEndZip64HdrPos := 0;
  // Look for end of central directory record from
  // back of file based on signature (only way due to
  // variable length zip comment etc)
  FFileComment := '';
  // Zip file requires end of central dir header so
  // is corrupt if it is smaller than that
  if FZipStream.Size < SizeOf(AEndHdr) then
  begin
    ZeroData;
    exit;
  end;

  AEndHdrPos := FZipStream.Size - SizeOf(AEndHdr);
  FZipStream.Seek(AEndHdrPos, soBeginning);
  FZipStream.ReadBuffer(AEndHdr, SizeOf(AEndHdr));
  {$IFDEF FPC_BIG_ENDIAN}
  AEndHdr := SwapECD(AEndHdr);
  {$ENDIF}
  // Search unless record is right at the end of the file:
  if (AEndHdr.Signature <> END_OF_CENTRAL_DIR_SIGNATURE) or (AEndHdr.ZipFile_Comment_Length <> 0) then
    SearchForSignature;
  if AEndHdrPos = 0 then
  begin
    ZeroData;
    exit;
  end;

  // With a valid end of dir record, see if there's zip64
  // fields:
  FZipStream.Seek(AEndHdrPos - SizeOf(Zip64_End_of_Central_Dir_Locator_type), soBeginning);
  FZipStream.ReadBuffer(EndZip64Locator, SizeOf(EndZip64Locator));
  {$IFDEF FPC_BIG_ENDIAN}
  EndZip64Locator := SwapZ64ECDL(EndZip64Locator);
  {$ENDIF}
  if EndZip64Locator.Signature = ZIP64_END_OF_CENTRAL_DIR_LOCATOR_SIGNATURE then
  begin
    //Read EndZip64Locator.Total_Disks when implementing multiple disks support
    if EndZip64Locator.Central_Dir_Zip64_EOCD_Offset > High(int64) then
      raise EZipError.CreateFmt(SErrPosTooLarge, [EndZip64Locator.Central_Dir_Zip64_EOCD_Offset, High(int64)]);
    AEndZip64HdrPos := EndZip64Locator.Central_Dir_Zip64_EOCD_Offset;
    FZipStream.Seek(AEndZip64HdrPos, soBeginning);
    FZipStream.ReadBuffer(AEndZip64Hdr, SizeOf(AEndZip64Hdr));
    {$IFDEF FPC_BIG_ENDIAN}
    AEndZip64Hdr := SwapZ64ECD(AEndZip64Hdr);
    {$ENDIF}
    if AEndZip64Hdr.Signature <> ZIP64_END_OF_CENTRAL_DIR_SIGNATURE then
    begin
      //Corrupt header
      ZeroData;
      Exit;
    end;
  end
  else
  begin
    // No zip64 data, so follow the offset in the end of central directory record
    AEndZip64HdrPos := 0;
    FillChar(AEndZip64Hdr, SizeOf(AEndZip64Hdr), 0);
  end;
end;

procedure TplZipUnCompress.ReadZipDirectory;
var
  EndHdr: End_of_Central_Dir_Type;
  EndZip64Hdr: Zip64_End_of_Central_Dir_type;
  i: integer; //could be Qword but limited to number of items in collection
  EndHdrPos, EndZip64HdrPos, CenDirPos, SavePos: int64; //could be QWord but limited to stream maximums
  ExtraFieldHeader: Extensible_Data_Field_Header_Type;
  EntriesThisDisk: QWord;
  Zip64Field: Zip64_Extended_Info_Field_Type;
  NewNode: TFullZipFileEntry;
  D: TDateTime;
  S: string;
  U: UTF8String;
  // infozip unicode path
  Infozip_unicode_path_ver: byte; // always 1
  Infozip_unicode_path_crc32: DWord;
begin
  FindEndHeaders(EndHdr, EndHdrPos,
    EndZip64Hdr, EndZip64HdrPos);
  if EndHdrPos = 0 then
    raise EZipError.CreateFmt(SErrCorruptZIP, [FileName]);
  if (EndZip64HdrPos > 0) and (EndZip64Hdr.Start_Disk_Offset > 0) then
  begin
    if EndZip64Hdr.Start_Disk_Offset > High(int64) then
      raise EZipError.CreateFmt(SErrPosTooLarge, [EndZip64Hdr.Start_Disk_Offset, High(int64)]);
    CenDirPos := EndZip64Hdr.Start_Disk_Offset;
  end
  else
    CenDirPos := EndHdr.Start_Disk_Offset;
  FZipStream.Seek(CenDirPos, soBeginning);
  FEntries.Clear;
  if (EndZip64HdrPos > 0) and (EndZip64Hdr.Entries_This_Disk > 0) then
  begin
    EntriesThisDisk := EndZip64Hdr.Entries_This_Disk;
    if EntriesThisDisk <> EndZip64Hdr.Total_Entries then
      raise EZipError.Create(SErrUnsupportedMultipleDisksCD);
  end
  else
  begin
    EntriesThisDisk := EndHdr.Entries_This_Disk;
    if EntriesThisDisk <> EndHdr.Total_Entries then
      raise EZipError.Create(SErrUnsupportedMultipleDisksCD);
  end;

  // Entries are added to a collection. The max number of items
  // in a collection limits the entries we can process.
  if EntriesThisDisk > MaxInt then
    raise EZipError.CreateFmt(SErrMaxEntries, [EntriesThisDisk, MaxInt]);

  // Using while instead of for loop so qword can be used on 32 bit as well.
  for i := 0 to EntriesThisDisk - 1 do
  begin
    FZipStream.ReadBuffer(CentralHdr, SizeOf(CentralHdr));
{$IFDEF FPC_BIG_ENDIAN}
    CentralHdr := SwapCFH(CentralHdr);
{$ENDIF}
    with CentralHdr do
    begin
      if Signature <> CENTRAL_FILE_HEADER_SIGNATURE then
        raise EZipError.CreateFmt(SErrCorruptZIP, [FileName]);
      NewNode := FEntries.Add as TFullZipFileEntry;
      // Header position will be corrected later with zip64 version, if needed..
      NewNode.HdrPos := Local_Header_Offset;
      NewNode.FBitFlags := Bit_Flag;
      SetLength(S, Filename_Length);
      FZipStream.ReadBuffer(S[1], Filename_Length);
      SavePos := FZipStream.Position; //After fixed part of central directory...
      // and the filename; before any extra field(s)
      NewNode.ArchiveFileName := S;
      // Size/compressed size will be adjusted by zip64 entries if needed...
      NewNode.Size := Uncompressed_Size;
      NewNode.FCompressedSize := Compressed_Size;
      NewNode.CRC32 := CRC32;
      NewNode.OS := MadeBy_Version shr 8;
      if NewNode.OS = OS_UNIX then
        NewNode.Attributes := External_Attributes shr 16
      else
        NewNode.Attributes := External_Attributes;
      ZipDateTimeToDateTime(Last_Mod_Date, Last_Mod_Time, D);
      NewNode.DateTime := D;

      // Go through any extra fields and extract any zip64 info
      if Extra_Field_Length > 0 then
      begin
        while (FZipStream.Position < SavePos + Extra_Field_Length) do
        begin
          FZipStream.ReadBuffer(ExtraFieldHeader, SizeOf(ExtraFieldHeader));
        {$IFDEF FPC_BIG_ENDIAN}
          ExtraFieldHeader := SwapEDFH(ExtraFieldHeader);
        {$ENDIF}
          if ExtraFieldHeader.Header_ID = ZIP64_HEADER_ID then
          begin
            FZipStream.ReadBuffer(Zip64Field, SizeOf(Zip64Field));
          {$IFDEF FPC_BIG_ENDIAN}
            Zip64Field := SwapZ64EIF(Zip64Field);
          {$ENDIF}
            if Zip64Field.Compressed_Size > 0 then
              NewNode.FCompressedSize := Zip64Field.Compressed_Size;
            if Zip64Field.Original_Size > 0 then
              NewNode.Size := Zip64Field.Original_Size;
            if Zip64Field.Relative_Hdr_Offset <> 0 then
            begin
              if Zip64Field.Relative_Hdr_Offset > High(int64) then
                raise EZipError.CreateFmt(SErrPosTooLarge, [Zip64Field.Relative_Hdr_Offset, High(int64)]);
              NewNode.HdrPos := Zip64Field.Relative_Hdr_Offset;
            end;
          end
          // infozip unicode path extra field
          else if ExtraFieldHeader.Header_ID = INFOZIP_UNICODE_PATH_ID then
          begin
            FZipStream.ReadBuffer(Infozip_unicode_path_ver, 1);
            if Infozip_unicode_path_ver = 1 then
            begin
              FZipStream.ReadBuffer(Infozip_unicode_path_crc32, sizeof(Infozip_unicode_path_crc32));
              {$IFDEF FPC_BIG_ENDIAN}
              Infozip_unicode_path_crc32 := SwapEndian(Infozip_unicode_path_crc32);
              {$ENDIF}
              if CRC32Str(S) = Infozip_unicode_path_crc32 then
              begin
                SetLength(U, ExtraFieldHeader.Data_Size - 5);
                FZipStream.ReadBuffer(U[1], Length(U));
                NewNode.UTF8ArchiveFileName := U;
              end
              else
                FZipStream.Seek(ExtraFieldHeader.Data_Size - 5, soFromCurrent);
            end
            else
              FZipStream.Seek(ExtraFieldHeader.Data_Size - 1, soFromCurrent);
          end
          else
          begin
            // Read past non-Zip64 extra field
            FZipStream.Seek(ExtraFieldHeader.Data_Size, soFromCurrent);
          end;
        end;
      end;
      // Move past extra fields and file comment to next header
      FZipStream.Seek(SavePos + Extra_Field_Length + File_Comment_Length, soFromBeginning);
    end;
  end;
end;

function TplZipUnCompress.CreateDeCompressor(Item: TZipFileEntry; AMethod: word; AZipFile, AOutFile: TStream): TZipDeCompressor;
begin
  case AMethod of
    8:
      Result := TZipInflater.Create(AZipFile, AOutFile, FBufSize);
    else
      raise EZipError.CreateFmt(SErrUnsupportedCompressionFormat, [AMethod]);
  end;
  FCurrenZipDeCompressor := Result;
end;

procedure TplZipUnCompress.UnZipOneFile(Item: TFullZipFileEntry);
var
  ZMethod: word;
{$ifdef unix}
  LinkTargetStream: TStringStream;
{$endif}
  OutputFileName: RawByteString;
  FOutStream: TStream;
  IsLink: boolean;
  IsCustomStream: boolean;
  U: UnicodeString;

  procedure SetAttributes;
  var
    Attrs: longint;
  begin
    // set attributes
    FileSetDate(OutputFileName, DateTimeToFileDate(Item.DateTime));
    if (Item.Attributes <> 0) then
    begin
      Attrs := 0;
      {$IFDEF UNIX}
      if (Item.OS in [OS_UNIX, OS_OSX]) then
        Attrs := Item.Attributes;
      if (Item.OS in [OS_FAT, OS_NTFS, OS_OS2, OS_VFAT]) then
        Attrs := ZipFatAttrsToUnixAttrs(Item.Attributes);
      {$ELSE}
      if (Item.OS in [OS_FAT, OS_NTFS, OS_OS2, OS_VFAT]) then
        Attrs := Item.Attributes;
      if (Item.OS in [OS_UNIX, OS_OSX]) then
        Attrs := ZipUnixAttrsToFatAttrs(ExtractFileName(Item.ArchiveFileName), Item.Attributes);
      {$ENDIF}
      if Attrs <> 0 then
      begin
        {$IFDEF UNIX}
        FpChmod(OutputFileName, Attrs);
        {$ELSE}
        FileSetAttr(OutputFileName, Attrs);
        {$ENDIF}
      end;
    end;
  end;

  procedure DoUnzip(const Dest: TStream);
  begin
    if ZMethod = 0 then
    begin
      if (LocalHdr.Compressed_Size <> 0) then
      begin
        if LocalZip64Fld.Compressed_Size > 0 then
          Dest.CopyFrom(FZipStream, LocalZip64Fld.Compressed_Size)
        else
          Dest.CopyFrom(FZipStream, LocalHdr.Compressed_Size);
        {$warning TODO: Implement CRC Check}
      end;
    end
    else
      with CreateDecompressor(Item, ZMethod, FZipStream, Dest) do
        try
          FTotPos := Self.FTotPos;
          FTotSize := Self.FTotSize;
          OnProgress := Self.OnProgress;
          OnProgressEx := Self.OnProgressEx;
          OnPercent := Self.OnPercent;
          OnProgress := Self.OnProgress;
          OnPercent := Self.OnPercent;
          DeCompress;
          Self.FTotPos := FTotPos;
          if Item.CRC32 <> Crc32Val then
            raise EZipError.CreateFmt(SErrInvalidCRC, [Item.ArchiveFileName]);
        finally
          FCurrenZipDeCompressor := nil;
          Free;
        end;
  end;

  procedure GetOutputFileName;
  var
    I: integer;

  begin
    if not UseUTF8 then
      OutputFileName := StringReplace(Item.DiskFileName, '/', DirectorySeparator, [rfReplaceAll])
    else
    begin
      // Sets codepage.
      OutputFileName := Item.UTF8DiskFileName;
      U := UTF8Decode(OutputFileName);
      // Do not use stringreplace, it will mess up the codepage.
      if '/' <> DirectorySeparator then
        for I := 1 to Length(U) do
          if U[i] = '/' then
            U[i] := DirectorySeparator;
      OutputFileName := UTF8Encode(U);
    end;
    if (not IsCustomStream) and (FOutputPath <> '') then
    begin
      // Do not use IncludeTrailingPathdelimiter
      OutputFileName := FOutputPath + OutputFileName;
    end;
  end;

begin
  ReadZipHeader(Item, ZMethod);
  if (Item.BitFlags and 1) <> 0 then
    raise EZipError.CreateFmt(SErrEncryptionNotSupported, [Item.ArchiveFileName]);
  if (Item.BitFlags and (1 shl 5)) <> 0 then
    raise EZipError.CreateFmt(SErrPatchSetNotSupported, [Item.ArchiveFileName]);
  // Normalize output filename to conventions of target platform.
  // Zip file always has / path separators
  IsCustomStream := Assigned(FOnCreateStream);
  GetOutputFileName;
  IsLink := Item.IsLink;
{$IFNDEF UNIX}
  if IsLink and not IsCustomStream then
  begin
    {$warning TODO: Implement symbolic link creation for non-unix, e.g.
    Windows NTFS}
    IsLink := False;
  end;
{$ENDIF}
  if IsCustomStream then
  begin
    try
      OpenOutput(OutputFileName, FOutStream, Item);
      if (IsLink = False) and (Item.IsDirectory = False) then
        DoUnzip(FOutStream);
    finally
      CloseOutput(Item, FOutStream);
    end;
  end
  else
  begin
    if IsLink then
    begin
      {$IFDEF UNIX}
      LinkTargetStream := TStringStream.Create('');
      try
        DoUnzip(LinkTargetStream);
        fpSymlink(PChar(LinkTargetStream.DataString), PChar(OutputFileName));
      finally
        LinkTargetStream.Free;
      end;
      {$ENDIF}
    end
    else if Item.IsDirectory then
      CreateDir(OutputFileName)
    else
    begin
      try
        OpenOutput(OutputFileName, FOutStream, Item);
        DoUnzip(FOutStream);
      finally
        CloseOutput(Item, FOutStream);
      end;
    end;
    SetAttributes;
  end;
end;

function TplZipUnCompress.IsMatch(I: TFullZipFileEntry): boolean;
begin
  if UseUTF8 then
    Result := (FFiles.IndexOf(I.UTF8ArchiveFileName) <> -1)
  else
    Result := (FFiles.IndexOf(I.ArchiveFileName) <> -1);
end;

function TplZipUnCompress.CalcTotalSize(AllFiles: boolean): int64;
var
  I: integer;
  Item: TFullZipFileEntry;
begin
  Result := 0;
  for i := 0 to FEntries.Count - 1 do
  begin
    Item := FEntries[i];
    if AllFiles or IsMatch(Item) then
      Result := Result + TZipFileEntry(Item).Size;
  end;
end;

procedure TplZipUnCompress.UnZipAllFiles;
var
  Item: TFullZipFileEntry;
  I: integer; //Really QWord but limited to FEntries.Count
  AllFiles: boolean;
begin
  FTerminated := False;
  FUnZipping := True;
  try
    AllFiles := (FFiles.Count = 0);
    OpenInput;
    try
      ReadZipDirectory;
      FTotPos := 0;
      FTotSize := CalcTotalSize(AllFiles);
      i := 0;
      while (I < FEntries.Count) and not Terminated do
      begin
        Item := FEntries[i];
        if AllFiles or IsMatch(Item) then
          UnZipOneFile(Item);
        Inc(I);
      end;
      if Assigned(FOnProgressEx) and not Terminated then
        FOnProgressEx(Self, FTotPos, FTotSize);
    finally
      CloseInput;
    end;
  finally
    FUnZipping := False;
  end;
end;

procedure TplZipUnCompress.SetBufSize(Value: longword);
begin
  if FUnZipping then
    raise EZipError.Create(SErrBufsizeChange);
  if Value >= DefaultBufSize then
    FBufSize := Value;
end;

procedure TplZipUnCompress.SetFileName(Value: RawByteString);
begin
  if FUnZipping then
    raise EZipError.Create(SErrFileChange);
  FFileName := Value;
end;

procedure TplZipUnCompress.SetOutputPath(Value: RawByteString);
var
  DS: RawByteString;
begin
  if FUnZipping then
    raise EZipError.Create(SErrFileChange);
  FOutputPath := Value;
  if (FOutputPath <> '') and (FoutputPath[Length(FoutputPath)] <> DirectorySeparator) then
  begin
    // Preserve codepage of outputpath
    DS := DirectorySeparator;
    SetCodePage(DS, StringCodePage(FoutputPath), False);
    FOutputPath := FoutputPath + DS;
  end;
end;

procedure TplZipUnCompress.UnZipFiles(AFileName: RawByteString; FileList: TStrings);
begin
  FFileName := AFileName;
  UNzipFiles(FileList);
end;

procedure TplZipUnCompress.UnZipFiles(FileList: TStrings);
begin
  FFiles.Assign(FileList);
  UnZipAllFiles;
end;

procedure TplZipUnCompress.UnZipAllFiles(AFileName: RawByteString);

begin
  FFileName := AFileName;
  UnZipAllFiles;
end;

procedure TplZipUnCompress.DoEndOfFile;
var
  ComprPct: double;
  Uncompressed: QWord;
  Compressed: QWord;
begin
  if LocalZip64Fld.Original_Size > 0 then
    Uncompressed := LocalZip64Fld.Original_Size
  else
    Uncompressed := LocalHdr.Uncompressed_Size;

  if LocalZip64Fld.Compressed_Size > 0 then
    Compressed := LocalZip64Fld.Compressed_Size
  else
    Compressed := LocalHdr.Compressed_Size;

  if (Compressed > 0) and (Uncompressed > 0) then
    if (Compressed > Uncompressed) then
      ComprPct := (-100.0 * (Compressed - Uncompressed)) / Uncompressed
    else
      ComprPct := (100.0 * (Uncompressed - Compressed)) / Uncompressed
  else
    ComprPct := 0;
  if Assigned(FOnEndOfFile) then
    FOnEndOfFile(Self, ComprPct);
end;

procedure TplZipUnCompress.Clear;

begin
  FFiles.Clear;
  FEntries.Clear;
end;

procedure TplZipUnCompress.Examine;
begin
  if (FOnOpenInputStream = nil) and (FFileName = '') then
    raise EZipError.Create(SErrNoFileName);
  OpenInput;
  if (FZipStream = nil) then
    raise EZipError.Create(SErrNoStream);
  try
    ReadZipDirectory;
  finally
    CloseInput;
  end;
end;

procedure TplZipUnCompress.Terminate;
begin
  FTerminated := True;
  if Assigned(FCurrenZipDeCompressor) then
    FCurrenZipDeCompressor.Terminate;
end;


//=============== TZipFileEntry ==============================

function TZipFileEntry.GetArchiveFileName: string;
begin
  Result := FArchiveFileName;
  if (Result = '') then
    Result := FDiskFileName;
end;

function TZipFileEntry.GetUTF8ArchiveFileName: UTF8String;
begin
  Result := FUTF8FileName;
  if Result = '' then
    Result := ArchiveFileName;
end;

function TZipFileEntry.GetUTF8DiskFileName: UTF8String;
begin
  Result := FUTF8DiskFileName;
  if Result = '' then
    Result := DiskFileName;
end;

constructor TZipFileEntry.Create(ACollection: TCollection);

begin
{$IFDEF UNIX}
  FOS := OS_UNIX;
{$ELSE}
  FOS := OS_FAT;
{$ENDIF}
  FCompressionLevel := cldefault;
  FDateTime := now;
  FNeedsZip64 := False;
  FAttributes := 0;

  inherited Create(ACollection);
end;

function TZipFileEntry.IsDirectory: boolean;
begin
  Result := (DiskFileName <> '') and (DiskFileName[Length(DiskFileName)] = DirectorySeparator);
  if Attributes <> 0 then
  begin
    case OS of
      OS_FAT: Result := (faDirectory and Attributes) > 0;
      OS_UNIX: Result := (Attributes and UNIX_MASK) = UNIX_DIR;
    end;
  end;
end;

function TZipFileEntry.IsLink: boolean;
begin
  Result := False;
  if Attributes <> 0 then
  begin
    case OS of
      OS_FAT: Result := (faSymLink and Attributes) > 0;
      OS_UNIX: Result := (Attributes and UNIX_MASK) = UNIX_LINK;
    end;
  end;
end;

procedure TZipFileEntry.SetArchiveFileName(const AValue: string);

begin
  if FArchiveFileName = AValue then
    Exit;
  // Zip standard: filenames inside the zip archive have / path separator
  if DirectorySeparator = '/' then
    FArchiveFileName := AValue
  else
    FArchiveFileName := StringReplace(AValue, DirectorySeparator, '/', [rfReplaceAll]);
end;

procedure TZipFileEntry.SetDiskFileName(const AValue: string);
begin
  if FDiskFileName = AValue then
    Exit;
  // Zip file uses / as directory separator on all platforms
  // so convert to separator used on current OS
  if DirectorySeparator = '/' then
    FDiskFileName := AValue
  else
    FDiskFileName := StringReplace(AValue, '/', DirectorySeparator, [rfReplaceAll]);
end;

procedure TZipFileEntry.SetUTF8ArchiveFileName(AValue: UTF8String);
begin
  FUTF8FileName := AValue;
  if ArchiveFileName = '' then
    if DefaultSystemCodePage <> CP_UTF8 then
      ArchiveFileName := Utf8ToAnsi(AValue)
    else
      ArchiveFileName := AValue;
end;

procedure TZipFileEntry.SetUTF8DiskFileName(AValue: UTF8String);
begin
  FUTF8DiskFileName := AValue;
  if DiskFileName = '' then
    if DefaultRTLFileSystemCodePage <> CP_UTF8 then
      DiskFileName := Utf8ToAnsi(AValue)
    else
      DiskFileName := AValue;
end;


procedure TZipFileEntry.Assign(Source: TPersistent);

var
  Z: TZipFileEntry;

begin
  if Source is TZipFileEntry then
  begin
    Z := Source as TZipFileEntry;
    FArchiveFileName := Z.FArchiveFileName;
    FDiskFileName := Z.FDiskFileName;
    FSize := Z.FSize;
    FDateTime := Z.FDateTime;
    FStream := Z.FStream;
    FOS := Z.OS;
    FAttributes := Z.Attributes;
  end
  else
    inherited Assign(Source);
end;

//================= TZipFileEntries ============================================

function TZipFileEntries.GetZ(AIndex: integer): TZipFileEntry;
begin
  Result := TZipFileEntry(Items[AIndex]);
end;

procedure TZipFileEntries.SetZ(AIndex: integer; const AValue: TZipFileEntry);
begin
  Items[AIndex] := AValue;
end;

function TZipFileEntries.AddFileEntry(const ADiskFileName: string): TZipFileEntry;
begin
  Result := Add as TZipFileEntry;
  Result.DiskFileName := ADiskFileName;
end;

function TZipFileEntries.AddFileEntry(const ADiskFileName, AArchiveFileName: string): TZipFileEntry;
begin
  Result := AddFileEntry(ADiskFileName);
  Result.ArchiveFileName := AArchiveFileName;
end;

function TZipFileEntries.AddFileEntry(const AStream: TSTream; const AArchiveFileName: string): TZipFileEntry;
begin
  Result := Add as TZipFileEntry;
  Result.Stream := AStream;
  Result.ArchiveFileName := AArchiveFileName;
end;

procedure TZipFileEntries.AddFileEntries(const List: TStrings);

var
  I: integer;

begin
  for I := 0 to List.Count - 1 do
    AddFileEntry(List[i]);
end;

//================= TFullZipFileEntries ===========================

function TFullZipFileEntries.GetFZ(AIndex: integer): TFullZipFileEntry;
begin
  Result := TFullZipFileEntry(Items[AIndex]);
end;

procedure TFullZipFileEntries.SetFZ(AIndex: integer; const AValue: TFullZipFileEntry);
begin
  Items[AIndex] := AValue;
end;

end.
