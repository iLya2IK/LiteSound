unit OGLRIFFWaveWrapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OGLSoundUtils, OGLSoundUtilTypes,
  fpwavformat,
  BufferedStream, OGLFastList,
  OGLOGGWrapper;

type
  IRIFFComment = interface(IVorbisComment)
  ['{073ABEF2-FAC3-43FD-A90A-105F8DB86499}']
  function WriteToMemory(Stream : TCustomMemoryStream) : Integer;
  function WriteToDataStream(Stream : ISoundDataStream) : Integer;
  function ReadFromDataStream(Stream : ISoundDataStream) : Integer;
  end;

  { TRIFFComment }

  TRIFFComment = class(TVorbisComment, IRIFFComment)
  private
    const AUDIO_CHUNK_ID_LIST = 'LIST';
    const META_INAM = 'INAM'; //TRACK TITLE
    const META_IPRD = 'IPRD'; //ALBUM TITLE
    const META_IART = 'IART'; //ARTIST NAME
    const META_ICMT = 'ICMT'; //COMMENTS
    const META_ICRD = 'ICRD'; //YEAR
    const META_IGNR = 'IGNR'; //GENRE
    const META_ITRK = 'ITRK'; //TRACK NUMBER
    const META_ISFT = 'ISFT'; //encoder
    type tWavCommPair = record
        ID : TChunkID;
        VorbisID : Cardinal;
      end;
    const cCommPairs : Array [0..7] of tWavCommPair = (
    (ID : META_INAM; VorbisID : TOGLSoundComments.COMMENT_TITLE;),
    (ID : META_IPRD; VorbisID : TOGLSoundComments.COMMENT_ALBUM;),
    (ID : META_IART; VorbisID : TOGLSoundComments.COMMENT_ARTIST;),
    (ID : META_ICRD; VorbisID : TOGLSoundComments.COMMENT_DATE;),
    (ID : META_IGNR; VorbisID : TOGLSoundComments.COMMENT_GENRE;),
    (ID : META_ITRK; VorbisID : TOGLSoundComments.COMMENT_TRACK;),
    (ID : META_ICMT; VorbisID : TOGLSoundComments.COMMENT_COMMENT;),
    (ID : META_ISFT; VorbisID : TOGLSoundComments.COMMENT_ENCODER;)
    );
  public
    function WriteToMemory(Stream : TCustomMemoryStream) : Integer;
    function WriteToDataStream(Stream : ISoundDataStream) : Integer;
    function ReadFromMemory(Stream : TCustomMemoryStream) : Integer;
    function ReadFromDataStream(Stream : ISoundDataStream) : Integer;
  end;

  { TWaveRIFFHeader }

  TWaveRIFFHeader = class
  private
    FWavFormat : TWaveFormat;
  public
    procedure Init(aProps : ISoundEncoderProps);
    function DoWriteHeader(aHeader : PChar; aDataSize : Int64) : Boolean;
    function DoReadHeader(aHeader : PChar) : Boolean;

    function GetBitrate : Cardinal;
    function GetChannels : Cardinal;
    function GetFrequency : Cardinal;
    function GetBitdepth : Cardinal;
    function GetVersion : Integer;
    function TotalSize  : Cardinal;

    const HEADER_SIZE = Sizeof(TWaveFormat) + Sizeof(TRiffHeader);
  end;

  { TRawEncoder }

  TRawEncoder = class(TSoundAbstractEncoder, ISoundStreamEncoder)
  private
    FWavFormat : TWaveRIFFHeader;
  protected
    function GetMode : TSoundEncoderMode; override;
    function GetQuality : Single; override;

    procedure Init(aProps : ISoundEncoderProps;
                   {%H-}aComment : ISoundComment); override;
    function GetBitrate : Cardinal; override;
    function GetChannels : Cardinal; override;
    function GetFrequency : Cardinal; override;
    function GetBitdepth : Cardinal; override;
    function GetVersion : Integer; override;
    procedure DoWriteRIFFHeader(Sz : Int64);
    procedure Done; override;
  public
    constructor Create(aStream : TStream;
                       aProps : ISoundEncoderProps);
    destructor Destroy; override;

    function Comments : ISoundComment; override;

    function Ready : Boolean; override;
    function  WriteData(Buffer : Pointer;
                   Count : ISoundFrameSize;
                   {%H-}Par : Pointer) : ISoundFrameSize; override;
    procedure WriteHeader({%H-}Par : Pointer); override;

    procedure SetStream(aStream : TStream);
  end;

  { TWavOggEncoder }

  TWavOggEncoder = class(TRawEncoder)
  private
    FOggStream : IOGGStreamState;
    FComments  : ISoundComment;
    FCurGranulePos : Int64;
    FCurPacketNum : Int64;
    FSampleChunkSize : ISoundFrameSize;
  protected
    procedure Init(aProps : ISoundEncoderProps;
                   aComment : ISoundComment); override;
    procedure Done; override;
    procedure WriteOggStream(aFlush : Boolean);
    procedure WriteEOS;
    procedure WriteOggHeader(id : Integer);
  public
    constructor Create(aStream : TStream;
                       aDataLimits : TSoundDataLimits;
                       aProps : ISoundEncoderProps;
                       aComments : ISoundComment);
    function  WriteData(Buffer : Pointer;
                   Count : ISoundFrameSize;
                   {%H-}Par : Pointer) : ISoundFrameSize; override;
    procedure WriteHeader({%H-}Par : Pointer); override;
    procedure Close({%H-}Par : Pointer); override;
    procedure Flush({%H-}Par : Pointer); override;
  end;

  { TRIFFWaveEncoder }

  TRIFFWaveEncoder = class(TRawEncoder)
  private
    FComments  : ISoundComment;
    FChunkSize : Int64;
    FLastDataChunkPos : Int64;
  protected
    procedure Init(aProps : ISoundEncoderProps;
                   aComment : ISoundComment); override;
    procedure DoWriteDataChunkSize;
  public
    constructor Create(aStream : TStream;
                       aProps : ISoundEncoderProps;
                       aComments : ISoundComment);

    function Comments : ISoundComment; override;

    function  WriteData(Buffer : Pointer;
                   Count : ISoundFrameSize;
                   {%H-}Par : Pointer) : ISoundFrameSize; override;
    procedure WriteHeader({%H-}Par : Pointer); override;
    procedure Close({%H-}Par : Pointer); override;
    procedure Flush({%H-}Par : Pointer); override;
  end;

  { TRawDecoder }

  TRawDecoder = class(TSoundAbstractDecoder, ISoundStreamDecoder)
  private
    type pRawDataChunk = ^tRawDataChunk;
         tRawDataChunk = record
          FRawPos : Cardinal;
          FSize   : Cardinal;
          FNext   : pRawDataChunk;
        end;
  private
    FDataChunks : pRawDataChunk;
    FCurIntPos  : Cardinal;
    FWavFormat : TWaveRIFFHeader;
    FReady : Boolean;
    function ReadHeader : Boolean;
    class function DefaultDataLimits : TSoundDataLimits; virtual;
  protected
    procedure Init; override;
    procedure Done; override;

    function GetBitdepth : Cardinal; override;
    function GetBitrate : Cardinal; override;
    function GetChannels : Cardinal; override;
    function GetFrequency : Cardinal; override;
    function GetVersion : Integer; override;
  public
    constructor Create(aStream : TStream);
    destructor Destroy; override;

    function  Comments : ISoundComment; override;

    function ReadData(Buffer : Pointer; Count : ISoundFrameSize;
                       {%H-}Par : Pointer) : ISoundFrameSize; override;

    procedure SetStream(aStream : TStream);

    function Ready : Boolean; override;
  end;

  { TRIFFWaveDecoder }

  TRIFFWaveDecoder = class(TRawDecoder)
  private
    FCurChunk   : pRawDataChunk;
    FDataPos    : Int64;
    FDataTotal  : Int64;
    FComments   : ISoundComment;
    function ParseStream : Boolean;
    class function DefaultDataLimits : TSoundDataLimits; override;
  protected
    procedure Init; override;
  public
    function  Comments : ISoundComment; override;

    function ReadData(Buffer : Pointer; Count : ISoundFrameSize;
                       {%H-}Par : Pointer) : ISoundFrameSize; override;
    procedure ResetToStart; override;
    procedure RawSeek(pos : Int64); override;
    procedure SampleSeek(pos : Integer); override;
    procedure TimeSeek(pos : Double); override;
    function RawTell : Int64; override;
    function SampleTell : Integer; override;
    function TimeTell : Double; override;
    function RawTotal : Int64; override;
    function SampleTotal : Integer; override;
    function TimeTotal : Double; override;
  end;

  { TWavOggDecoder }

  TWavOggDecoder = class(TSoundAbstractDecoder, ISoundStreamDecoder)
  private
    type pLinkedPage = ^tLinkedPage;
         tLinkedPage = record
          FOffset   : Int64;
          FSize     : Cardinal;
          FSerialNo : Int64;
          FGranule  : Int64;
        end;
  private
    FComments   : ISoundComment;
    FStartPos   : Int64;

    FOggSync    : IOGGSyncState;
    FOggStream  : IOGGStreamState;
    FLinked     : TFastPointerCollection;
    FCurLink    : Integer;
    FCurIntPos  : Int32;
    FLastGranule: Int64;
    FCurPage    : IOGGPage;
    FCurPacket  : IOGGPacket;
    //

    FWavFormat : TWaveRIFFHeader;
    FReady : Boolean;

    function OggPosition : Int64;
    function ReadHeader : Boolean;
    function FindLinkBisect(sample : Integer) : Integer;
    procedure OggStreamSeekToPos(bytepos : Integer);
    function OggPreparePage(linkno : integer) : Boolean;
    function OggGetData(_nbytes : Int32) : Int32;
    function OggGetNextPage(og : IOGGPage; var offset : Int64) : Int64;
    function ParseAllStream : Boolean;
    function CurLink : pLinkedPage;
    function LinkAt(pos : integer) : pLinkedPage;
  protected
    procedure Init; override;
    procedure Done; override;

    class function DefaultOggBufferSize : Uint32; virtual;

    function GetBitdepth : Cardinal; override;
    function GetBitrate : Cardinal; override;
    function GetChannels : Cardinal; override;
    function GetFrequency : Cardinal; override;
    function GetVersion : Integer; override;
  public
    constructor Create(aStream : TStream; aDataLimits : TSoundDataLimits);
    destructor Destroy; override;

    function  Comments : ISoundComment; override;

    function ReadData(Buffer : Pointer; Count : ISoundFrameSize;
                       {%H-}Par : Pointer) : ISoundFrameSize; override;

    procedure SetStream(aStream : TStream);
    function Ready : Boolean; override;

    procedure ResetToStart; override;
    procedure RawSeek(pos : Int64); override;
    procedure SampleSeek(pos : Integer); override;
    procedure TimeSeek(pos : Double); override;
    function RawTell : Int64; override;
    function SampleTell : Integer; override;
    function TimeTell : Double; override;
    function RawTotal : Int64; override;
    function SampleTotal : Integer; override;
    function TimeTotal : Double; override;
  end;

  { TRIFFWaveFile }

  TRIFFWaveFile = class(TSoundFile)
  protected
    function InitEncoder(aProps : ISoundEncoderProps;
                   aComments : ISoundComment) : ISoundEncoder; override;
    function InitDecoder : ISoundDecoder; override;
  end;

  { TOggWaveFile }

  TOggWaveFile = class(TSoundFile)
  protected
    function InitEncoder(aProps : ISoundEncoderProps;
                   aComments : ISoundComment) : ISoundEncoder; override;
    function InitDecoder : ISoundDecoder; override;
  end;

  { TWAVE }

  TWAVE = class
  public
    class function NewComment : ISoundComment;
    class function NewComment(aSrc : ISoundComment) : ISoundComment;
    class function NewOggStreamEncoder(aStream : TStream;
        aDataLimits : TSoundDataLimits; aProps : ISoundEncoderProps;
        aComments : ISoundComment) : ISoundStreamEncoder;
    class function NewOggStreamDecoder(aStream : TStream;
        aDataLimits : TSoundDataLimits) : ISoundStreamDecoder;
    class function NewStreamEncoder(aStream : TStream;
        aDataLimits : TSoundDataLimits; aProps : ISoundEncoderProps;
        aComments : ISoundComment) : ISoundStreamEncoder;
    class function NewStreamDecoder(aStream : TStream;
        aDataLimits : TSoundDataLimits) : ISoundStreamDecoder;

    class function EncoderVersionString : String;
  end;

implementation

const RIFF_CODEC_VERSION = 1;

procedure NtoLE(var fmt: TWaveFormat); overload;
begin
  with fmt, ChunkHeader do begin
    Size := NtoLE(Size);
    Format := NtoLE(Format);
    Channels := NtoLE(Channels);
    SampleRate := NtoLE(SampleRate);
    ByteRate := NtoLE(ByteRate);
    BlockAlign := NtoLE(BlockAlign);
    BitsPerSample := NtoLE(BitsPerSample);
  end;
end;

procedure LEtoN(var fmt: TWaveFormat); overload;
begin
  with fmt, ChunkHeader do begin
    Size := LEtoN(Size);
    Format := LEtoN(Format);
    Channels := LEtoN(Channels);
    SampleRate := LEtoN(SampleRate);
    ByteRate := LEtoN(ByteRate);
    BlockAlign := LEtoN(BlockAlign);
    BitsPerSample := LEtoN(BitsPerSample);
  end;
end;

{ TWaveRIFFHeader }

procedure TWaveRIFFHeader.Init(aProps : ISoundEncoderProps);
begin
  FWavFormat.ChunkHeader.ID := AUDIO_CHUNK_ID_fmt;
  FWavFormat.ChunkHeader.Size := SizeOf(FWavFormat) - SizeOf(TChunkHeader);
  FWavFormat.Format := WAVE_FORMAT_PCM;
  FWavFormat.Channels := aProps.Channels;
  FWavFormat.SampleRate := aProps.Frequency;
  FWavFormat.BitsPerSample := TOGLSound.SampleSizeToBitdepth(aProps.SampleSize);
  FWavFormat.ByteRate := (FWavFormat.SampleRate * FWavFormat.BitsPerSample * FWavFormat.Channels) div 8;
  FWavFormat.BlockAlign := (FWavFormat.BitsPerSample * FWavFormat.Channels) div 8;
end;

function TWaveRIFFHeader.DoWriteHeader(aHeader : PChar; aDataSize : Int64
  ) : Boolean;
var
  riff: TRiffHeader;
  fmtLE: TWaveFormat;
begin
  with riff, ChunkHeader do begin
    ID := AUDIO_CHUNK_ID_RIFF;
    Size := NtoLE(Cardinal(aDataSize));
    Format := AUDIO_CHUNK_ID_WAVE;
  end;
  fmtLE := FWavFormat;
  NtoLE(fmtLE);
  Move(PByte(@riff)^, aHeader^, Sizeof(riff));
  Move(PByte(@fmtLE)^, PByte(aHeader)[SizeOf(riff)], Sizeof(fmtLE));
  Result := true;
end;

function TWaveRIFFHeader.DoReadHeader(aHeader : PChar) : Boolean;
var
  riff: TRiffHeader;
begin
  Move(aHeader^, riff, sizeof(riff));
  riff.ChunkHeader.Size := LEtoN(riff.ChunkHeader.Size);

  if (riff.ChunkHeader.ID = AUDIO_CHUNK_ID_RIFF) and (riff.Format = AUDIO_CHUNK_ID_WAVE) then
  begin
    Move(PByte(aHeader)[sizeof(riff)], FWavFormat, sizeof(FWavFormat));
    LEtoN(FWavFormat);
    if (FWavFormat.ChunkHeader.ID = AUDIO_CHUNK_ID_fmt) and
       ((FWavFormat.ChunkHeader.Size + 8) >= sizeof(FWavFormat)) then
    begin
      Result := true;
    end else
      Result := false;
  end else
    Result := false;
end;

function TWaveRIFFHeader.GetBitrate : Cardinal;
begin
  Result := FWavFormat.SampleRate * GetBitdepth * GetChannels;
end;

function TWaveRIFFHeader.GetChannels : Cardinal;
begin
  Result := FWavFormat.Channels;
end;

function TWaveRIFFHeader.GetFrequency : Cardinal;
begin
  Result := FWavFormat.SampleRate;
end;

function TWaveRIFFHeader.GetBitdepth : Cardinal;
begin
  Result := FWavFormat.BitsPerSample;
end;

function TWaveRIFFHeader.GetVersion : Integer;
begin
  Result := RIFF_CODEC_VERSION;
end;

function TWaveRIFFHeader.TotalSize : Cardinal;
begin
  Result := FWavFormat.ChunkHeader.Size;
end;

{ TWavOggDecoder }

function TWavOggDecoder.OggPosition : Int64;
begin
  Result := DataStream.DoTell - FOggSync.Ref^.fill + FOggSync.Ref^.returned;
end;

function TWavOggDecoder.ReadHeader : Boolean;
const OGG_HEADER_CHUNK = 128;
var
  sz : Integer;
  Opage : IOGGPage;
  Opack : IOGGPacket;

  BS : TBufferedStream;
  buffer : Pointer;
  headernum : integer;
begin
  headernum := 0;
  Result := true;
  Opage := TOGG.NewPage;
  while Result and (headernum < 2) do
  begin
    buffer := FOggSync.Buffer(OGG_HEADER_CHUNK);
    sz := DataStream.DoRead(buffer, OGG_HEADER_CHUNK);
    if sz > 0 then
    begin
      FOggSync.Wrote(sz);
      if (FOggSync.PageOut(Opage) = 1) then
      begin
        if (FOggSync.Check = 0) and
            Assigned(Opage) and
            ((Opage.Ref^.header_len > 0) or (Opage.Ref^.body_len > 0)) and
            (Opage.Packets > 0) then
        begin
          if not Assigned(FOggStream) then
            FOggStream := TOGG.NewStream(Opage.SerialNo);
          Opack := TOGG.NewPacket;
          if (FOggStream.PageInIgnoreErrors(Opage) = 0) and
              FOggStream.PacketOut(Opack) and
              (Opack.Size > 1) then
          begin
            case headernum of
              0 :
                Result := FWavFormat.DoReadHeader(Opack.Data);

              1 : begin
                BS := TBufferedStream.Create;
                try
                  BS.SetPtr(Opack.Data, Opack.Size);
                  Result := (FComments as TRIFFComment).ReadFromMemory(BS) > 0;
                finally
                  BS.Free;
                end;
              end;
            end;
            if Result then
              Inc(headernum);

          end else
            Result := false;
        end else
          Result := false;
      end;
    end else
      Result := false;
  end;
  if Result and DataStream.Seekable then
  begin
    FStartPos := OggPosition;
    DataStream.DoSeek(FStartPos, 0);
    FOggSync.Reset;
    Result := ParseAllStream;
  end;
end;

function TWavOggDecoder.FindLinkBisect(sample : Integer) : Integer;
var mip, map, p : integer;
    mis, mas, s : integer;
begin
  if FLinked.Count > 0 then
  begin
    map := FLinked.Count-1;
    mas := LinkAt(map)^.FGranule;
    if sample >= mas then Exit(-1);

    mip := 0;
    mis := LinkAt(mip)^.FGranule;
    if sample < mis then Exit(mip);

    while ((map - mip) > 1) do
    begin
      p := (mip + map) div 2;
      s := LinkAt(p)^.FGranule;
      if s = sample then
      begin
        Exit(p + 1);
      end else
      if s > sample then
      begin
        mas := s;
        map := p;
      end else
      begin
        mis := s;
        mip := p;
      end;
    end;
    Result := map;
  end else
    Exit(-1);
end;

procedure TWavOggDecoder.OggStreamSeekToPos(bytepos : Integer);
var
  p : Int64;
  ret : integer;
begin
  FCurPacket := TOGG.NewPacket;
  p := 0;
  while true do
  begin
    ret := FOggStream.PacketOutIgnoreErrors(FCurPacket);

    if (ret = 1) then
    begin
      if (FCurPacket.Size + p) > bytepos then
      begin
        FCurIntPos := bytepos - p;
        Break;
      end;
      Inc(p, FCurPacket.Size);
    end else
    if ret = 0 then
    begin
      FCurPacket := nil;
      FCurIntPos := 0;
      Break;
    end;
  end;
end;

function TWavOggDecoder.OggPreparePage(linkno : integer) : Boolean;
var
  ret : Int32;
  link : pLinkedPage;
begin
  FCurLink := linkno;
  link := CurLink;
  FCurPacket := nil;

  if Assigned(link) then
  begin
    if linkno > 0 then
      FLastGranule := pLinkedPage(FLinked[linkno-1])^.FGranule else
      FLastGranule := 0;

    FOggSync.Reset;
    FOggStream.ResetSerialNo(link^.FSerialNo);
    DataStream.DoSeek(link^.FOffset, 0);
    FCurIntPos := 0;

    FCurPage := TOGG.NewPage;

    while true do
    begin
      ret := OggGetData(DefaultOggBufferSize);
      if (ret <= 0) then Exit(false);

      ret := FOggSync.PageOut(FCurPage);

      if (ret > 0) then begin
        if (FOggSync.Check = 0) and
            Assigned(FCurPage) and
            ((FCurPage.Ref^.header_len > 0) or (FCurPage.Ref^.body_len > 0)) and
            (FCurPage.Packets > 0) then
        begin
          Result := (FOggStream.PageInIgnoreErrors(FCurPage) = 0);
        end else
          Result := false;
        Break;
      end;
    end;
  end else
  begin
    Result := false;
    FCurPage := nil;
  end;
end;

function TWavOggDecoder.OggGetData(_nbytes : Int32) : Int32;
var
  buffer : PChar;
begin
  if (_nbytes <= 0) then Exit(-1);
  buffer := FOggSync.Buffer(_nbytes);
  if not Assigned(buffer) then Exit(-1);
  Result := DataStream.DoRead(buffer, _nbytes);
  if (Result>0) then FOggSync.Wrote(Result);
end;

function TWavOggDecoder.OggGetNextPage(og : IOGGPage; var offset : Int64) : Int64;
var
  more, read_nbytes, ret : Int32;
begin
  while true do
  begin
    more := FOggSync.PageSeek(og);
    { Skipped (-more) bytes. }
    if (more<0) then
      offset -= more
    else
    if (more=0) then
    begin
      read_nbytes := DefaultOggBufferSize;
      ret := OggGetData(read_nbytes);
      if (ret < 0) then Exit(-1);
      if (ret = 0) then
      begin
        {Only fail cleanly on EOF if we didn't have a known boundary.
          Otherwise, we should have been able to reach that boundary, and this
           is a fatal error.}
        Exit(-1);
      end;
    end
    else begin
      {Got a page.
        Return the page start offset and advance the internal offset past the
         page end.}
      Result := offset;
      offset += more;
      Exit;
    end;
  end;
end;

function TWavOggDecoder.ParseAllStream : Boolean;
var
  p : IOGGPage;
  offset, fromoff, ret : Int64;
  link : pLinkedPage;
  serialno : Integer;
  FTotalSize : Int64;
begin
  FLinked.Clear;

  p := TOGG.NewPage;
  FTotalSize := 0;
  offset := DataStream.DoTell;
  while (offset < DataStream.Size) do
  begin
    fromoff := offset;
    ret := OggGetNextPage(p, offset);
    if (ret < 0) then Exit(false)
    else if (ret = 0) then break;

    serialno := p.SerialNo;
    {Save the information for this page}

    link := GetMem(sizeof(tLinkedPage));

    link^.FOffset := fromoff;
    link^.FSerialNo := serialno;
    link^.FSize := Cardinal(offset - fromoff);
    link^.FGranule := p.GranulePos;
    Inc(FTotalSize, link^.FSize );
    FLinked.Add(link);
  end;
  if FLinked.Count > 0 then
  begin
    Result := OggPreparePage(0);
  end else
    Result := false;
end;

function TWavOggDecoder.CurLink : pLinkedPage;
begin
  if (FCurLink >= 0) and (FCurLink < FLinked.Count) then
    Result := LinkAt(FCurLink) else
    Result := nil;
end;

function TWavOggDecoder.LinkAt(pos : integer) : pLinkedPage;
begin
  Result := pLinkedPage(FLinked[pos]);
end;

procedure TWavOggDecoder.Init;
begin
  FWavFormat := TWaveRIFFHeader.Create;
  FComments := TWAVE.NewComment;

  FOggSync := TOGG.NewSyncState;
  FOggStream := nil;

  FStartPos := -1;
  FCurLink := -1;
  FCurIntPos := 0;
  FLastGranule := 0;
  FCurPage := nil;

  if DataStream.Seekable then
    FLinked := TFastPointerCollection.Create else
    FLinked := nil;
  FReady := ReadHeader;
end;

procedure TWavOggDecoder.Done;
begin
  FOggSync := nil;
  FOggStream := nil;
  FComments := nil;
  FreeAndNil(FWavFormat);
  if Assigned(FLinked) then
    FreeAndNil(FLinked);
end;

class function TWavOggDecoder.DefaultOggBufferSize : Uint32;
begin
  Result := 4096;
end;

function TWavOggDecoder.GetBitdepth : Cardinal;
begin
  Result := FWavFormat.GetBitdepth;
end;

function TWavOggDecoder.GetBitrate : Cardinal;
begin
  Result := FWavFormat.GetBitrate;
end;

function TWavOggDecoder.GetChannels : Cardinal;
begin
  Result := FWavFormat.GetChannels;
end;

function TWavOggDecoder.GetFrequency : Cardinal;
begin
  Result := FWavFormat.GetFrequency;
end;

function TWavOggDecoder.GetVersion : Integer;
begin
  Result := FWavFormat.GetVersion;
end;

constructor TWavOggDecoder.Create(aStream : TStream;
  aDataLimits : TSoundDataLimits);
begin
  InitStream(TOGLSound.NewDataStream(aStream,aDataLimits));
  Init;
end;

destructor TWavOggDecoder.Destroy;
begin
  Done;
  inherited Destroy;
end;

function TWavOggDecoder.Comments : ISoundComment;
begin
  Result := FComments;
end;

function TWavOggDecoder.ReadData(Buffer : Pointer; Count : ISoundFrameSize;
  Par : Pointer) : ISoundFrameSize;
var
  sz, total, p : Int64;
  ret : integer;
  cl : pLinkedPage;
begin
  total := Count.AsBytes;
  p := 0;
  while p < total do
  begin
    cl := CurLink;
    if Assigned(cl) then
    begin
       if Assigned(FCurPacket) then
       begin
         sz := FCurPacket.Size - Int64(FCurIntPos);
       end else
       begin
         FCurPacket := TOGG.NewPacket;
         repeat
           ret := FOggStream.PacketOutIgnoreErrors(FCurPacket);

           if (ret = 1) then
           begin
             FCurIntPos := 0;
             sz := FCurPacket.Size;
           end else
             sz := 0;
         until ret >= 0;
       end;

       if sz = 0 then
       begin
         if not OggPreparePage(FCurLink + 1) then
           break;
       end else
       begin
         if sz > (total - p) then sz := total - p;

         Move(PByte(FCurPacket.Data)[FCurIntPos],
              PByte(Buffer)[p], sz);
         Inc(p, sz);
         Inc(FCurIntPos, sz);

         if FCurPacket.Size = FCurIntPos then
           FCurPacket := nil;
       end;
    end else
    begin
      sz := 0;
      break;
    end;
  end;
  Result := Count.EmptyDuplicate;
  Result.IncBytes(p);
end;

procedure TWavOggDecoder.SetStream(aStream : TStream);
begin
  (DataStream as TSoundDataStream).Stream := aStream;
end;

function TWavOggDecoder.Ready : Boolean;
begin
  Result := FReady;
end;

procedure TWavOggDecoder.ResetToStart;
begin
  if (DataStream.Seekable) and (FStartPos >= 0) then
  begin
    DataStream.DoSeek(FStartPos, 0);
  end;
  FOggSync.Reset;
end;

procedure TWavOggDecoder.RawSeek(pos : Int64);
var s : Int32;
begin
  if DataStream.Seekable then
  begin
    s := FrameFromBytes(pos).AsSamples;
    SampleSeek(s);
  end
  else
    inherited RawSeek(pos);
end;

procedure TWavOggDecoder.SampleSeek(pos : Integer);
var linkno : Integer;
begin
  if DataStream.Seekable then
  begin
    linkno := FindLinkBisect(pos);
    if (linkno >= 0) and OggPreparePage(linkno) then
    begin
      OggStreamSeekToPos(FrameFromSamples(pos - FLastGranule).AsBytes)
    end
  end
  else
    inherited SampleSeek(pos);
end;

procedure TWavOggDecoder.TimeSeek(pos : Double);
var s : Int32;
begin
  if DataStream.Seekable then
  begin
    s := FrameFromDuration(pos * 1000.0).AsSamples;
    SampleSeek(s);
  end
  else
    inherited TimeSeek(pos);
end;

function TWavOggDecoder.RawTell : Int64;
begin
  if DataStream.Seekable then
  begin
    Result := FrameFromSamples(SampleTell).AsBytes;
  end
  else
    Result := inherited RawTell;
end;

function TWavOggDecoder.SampleTell : Integer;
begin
  if DataStream.Seekable then
  begin
    if FLastGranule >= 0 then
    begin
      Result := FLastGranule + FrameFromBytes(FCurIntPos).AsSamples;
    end;
  end
  else
    Result := inherited SampleTell;
end;

function TWavOggDecoder.TimeTell : Double;
begin
  if DataStream.Seekable then
  begin
    Result := FrameFromSamples(SampleTell).AsDurationSec;
  end
  else
    Result := inherited TimeTell;
end;

function TWavOggDecoder.RawTotal : Int64;
begin
  Result := Int64(FWavFormat.TotalSize);
end;

function TWavOggDecoder.SampleTotal : Integer;
var
  bpos : Int64;
begin
  bpos := RawTotal;
  Result := FrameFromBytes(bpos).AsSamples;
end;

function TWavOggDecoder.TimeTotal : Double;
var
  bpos : Int64;
begin
  bpos := RawTotal;
  Result := FrameFromBytes(bpos).AsDurationSec;
end;

{ TOggWaveFile }

function TOggWaveFile.InitEncoder(aProps : ISoundEncoderProps;
  aComments : ISoundComment) : ISoundEncoder;
begin
  Result := TWAVE.NewOggStreamEncoder(Stream, DataLimits, aProps,
                                         aComments as ISoundComment);
end;

function TOggWaveFile.InitDecoder : ISoundDecoder;
begin
  Result := TWAVE.NewOggStreamDecoder(Stream, DataLimits);
end;

{ TWavOggEncoder }

procedure TWavOggEncoder.Init(aProps : ISoundEncoderProps;
  aComment : ISoundComment);
const MAX_OGG_PACKET_SIZE = 8192;
begin
  FOggStream := TOGG.NewStream(Abs(Random(Int64(Now))));
  if Assigned(aComment) then
    FComments := aComment else
    FComments := TWAVE.NewComment;

  FCurGranulePos := 0;
  FCurPacketNum := 0;

  inherited Init(aProps, nil);

  if Ready then
  begin
   FSampleChunkSize := FrameFromSamples(FrameFromBytes(MAX_OGG_PACKET_SIZE).AsSamples);
  end else
   FSampleChunkSize := TOGLSound.NewErrorFrame;
end;

procedure TWavOggEncoder.Done;
begin
  FOggStream := nil;
  FComments := nil;
  inherited Done;
end;

procedure TWavOggEncoder.WriteOggStream(aFlush : Boolean);
begin
  if aFlush then
    FOggStream.PagesFlushToStream((DataStream as TSoundDataStream).Stream) else
    FOggStream.PagesOutToStream((DataStream as TSoundDataStream).Stream);
end;

procedure TWavOggEncoder.WriteEOS;
var
  og : IOGGPacket;
begin
  og := TOgg.NewEOSPacket(nil, 0, FCurPacketNum, FCurGranulePos);

  FOggStream.PacketIn(og);
end;

procedure TWavOggEncoder.WriteOggHeader(id : Integer);
var
  header: IOGGPacket;
  aHeader : PChar;
  aCommStr : TMemoryStream;
begin
  case id of
    0 : begin
      aHeader := GetMem(FWavFormat.HEADER_SIZE);
      try
        FWavFormat.DoWriteHeader(aHeader, 0);
        header := TOgg.NewBOSPacket(aHeader,
                                    FWavFormat.HEADER_SIZE,
                                    FCurPacketNum, FCurGranulePos);
        Inc(FCurPacketNum);

        fOggStream.PacketIn(header);

      finally
        FreeMem(aHeader);
      end;
    end;
    1 : begin
      aCommStr := TMemoryStream.Create;
      try
        (FComments as TRIFFComment).WriteToMemory(aCommStr);
        header := TOgg.NewPacket(aCommStr.Memory, aCommStr.Size,
                                      FCurPacketNum, FCurGranulePos);
        Inc(FCurPacketNum);

        fOggStream.PacketIn(header);
      finally
        aCommStr.Free;
      end;
    end;
  end;
end;

constructor TWavOggEncoder.Create(aStream : TStream;
  aDataLimits : TSoundDataLimits; aProps : ISoundEncoderProps;
  aComments : ISoundComment);
begin
  InitStream(TOGLSound.NewDataStream(aStream, aDataLimits));
  Init(aProps, aComments);
end;

function TWavOggEncoder.WriteData(Buffer : Pointer; Count : ISoundFrameSize;
  Par : Pointer) : ISoundFrameSize;
var
  og : IOGGPacket;
  Sz : Int64;

  ChunkSize : Int32;
begin
  sz := Count.AsBytes;

  while sz > 0 do
  begin
    ChunkSize := FSampleChunkSize.AsBytes;
    if Int64(ChunkSize) > sz then begin
      ChunkSize := sz;
      Inc(FCurGranulePos, FrameFromBytes(sz).AsSamples);
    end else
      Inc(FCurGranulePos, FSampleChunkSize.AsSamples);
    og := TOgg.NewPacket(Buffer, ChunkSize, FCurPacketNum, FCurGranulePos);

    Inc(FCurPacketNum);

    FOggStream.PacketIn(og);

    Dec(Sz, ChunkSize);
    Inc(PByte(Buffer), ChunkSize);
  end;
  WriteOggStream(true);

  Result := Count.Duplicate;
end;

procedure TWavOggEncoder.WriteHeader(Par : Pointer);
begin
  WriteOggHeader(0);
  WriteOggHeader(1);

  WriteOggStream(True);
end;

procedure TWavOggEncoder.Close(Par : Pointer);
var
  P, tmp_pn, tmp_gp : Int64;
  SZ : uint32;
begin
  WriteEOS;
  Flush(par);

  if DataStream.Seekable then
  begin
    SZ := NtoLE(Cardinal(FrameFromSamples(FCurGranulePos).AsBytes));
    FWavFormat.FWavFormat.ChunkHeader.Size := SZ;

    tmp_pn := FCurPacketNum;
    tmp_gp := FCurGranulePos;
    P := DataStream.DoTell;
    FCurPacketNum := 0;
    FCurGranulePos := 0;

    DataStream.DoSeek(0, 0);

    FOggStream.Reset;
    WriteOggHeader(0);
    WriteOggStream(true);

    FCurPacketNum := tmp_pn;
    FCurGranulePos := tmp_gp;

    DataStream.DoSeek(P, 0);
  end;
end;

procedure TWavOggEncoder.Flush(Par : Pointer);
begin
  WriteOggStream(True);
end;

{ TRIFFWaveDecoder }

function TRIFFWaveDecoder.ParseStream : Boolean;
var
  chnk : TChunkHeader;
  ch, rch : pRawDataChunk;
begin
  ch := nil;
  rch := nil;
  FDataTotal := 0;
  FDataPos := 0;
  while True do
  begin
    if DataStream.DoRead(@chnk, Sizeof(TChunkHeader) ) = Sizeof(TChunkHeader) then
    begin
      if chnk.ID = AUDIO_CHUNK_ID_data then
      begin
        ch := GetMem(Sizeof(tRawDataChunk));
        ch^.FRawPos := DataStream.DoTell;
        ch^.FSize := chnk.Size;
        ch^.FNext := nil;

        if not Assigned(FDataChunks) then
          FDataChunks := ch;
        if Assigned(rch) then
          rch^.FNext := ch;

        DataStream.DoSeek(chnk.Size, 1);

        rch := ch;

        Inc(FDataTotal, chnk.Size);
      end else
      if chnk.ID = TRIFFComment.AUDIO_CHUNK_ID_LIST then
      begin
        DataStream.DoSeek(-Sizeof(TChunkHeader), 1);
        (FComments as TRIFFComment).ReadFromDataStream(DataStream)
      end
      else
        Break;
    end else
      Break;
  end;
  Result := Assigned(FDataChunks);
  if Result then
    ResetToStart;
end;

class function TRIFFWaveDecoder.DefaultDataLimits : TSoundDataLimits;
begin
  Result := [sdpReadOnly];
end;

procedure TRIFFWaveDecoder.Init;
begin
  inherited Init;
  FCurIntPos := 0;
  FComments := TWAVE.NewComment;
  if FReady then
    FReady := ParseStream;
end;

function TRIFFWaveDecoder.Comments : ISoundComment;
begin
  Result := FComments;
end;

function TRIFFWaveDecoder.ReadData(Buffer : Pointer; Count : ISoundFrameSize;
  Par : Pointer) : ISoundFrameSize;
var
  sz, total, p : Cardinal;
begin
  total := Count.AsBytes;
  p := 0;
  while p < total do
  begin
    if Assigned(FCurChunk) then
    begin
      sz := Cardinal(Int64(FCurChunk^.FSize) - Int64(FCurIntPos));
    end else
      sz := 0;
    if sz = 0 then
    begin
      if Assigned(FCurChunk) then
      begin
        Inc(FDataPos, FCurChunk^.FSize);
        FCurChunk := FCurChunk^.FNext;
        if Assigned(FCurChunk) then
          DataStream.DoSeek(FCurChunk^.FRawPos, 0);
        FCurIntPos := 0;
      end
      else
        Break;
    end else
    begin
      if sz > (total - p) then sz := total - p;
      sz := DataStream.DoRead(@(PByte(Buffer)[p]), sz);
      if sz = 0 then Break;
      Inc(p, sz);
      Inc(FCurIntPos, sz);
    end;
  end;
  Result := Count.EmptyDuplicate;
  Result.IncBytes(p);
end;

procedure TRIFFWaveDecoder.ResetToStart;
begin
  FCurChunk := FDataChunks;
  FCurIntPos := 0;
  FDataPos := 0;
  if Assigned(FCurChunk) then
    DataStream.DoSeek(FCurChunk^.FRawPos, 0);
end;

procedure TRIFFWaveDecoder.RawSeek(pos : Int64);
var
  ch : pRawDataChunk;
  P : Int64;
begin
  ch := FDataChunks;
  P := 0;
  while Assigned(ch)  do
  begin
    if (Int64(ch^.FSize) + p) > pos then
    begin
      FCurChunk := ch;
      FCurIntPos := Cardinal(pos - p);
      FDataPos := p;
      DataStream.DoSeek(ch^.FRawPos + FCurIntPos, 0);
      Break;
    end else
    begin
      Inc(p, ch^.FSize);
      ch := ch^.FNext;
    end;
  end;
end;

procedure TRIFFWaveDecoder.SampleSeek(pos : Integer);
var
  bpos : Int64;
begin
  bpos := FrameFromSamples(pos).AsBytes;
  RawSeek(bpos);
end;

procedure TRIFFWaveDecoder.TimeSeek(pos : Double);
var
  bpos : Int64;
begin
  bpos := FrameFromDuration(pos * 1000.0).AsBytes;
  RawSeek(bpos);
end;

function TRIFFWaveDecoder.RawTell : Int64;
begin
  if Assigned(FCurChunk) then
    Result := FDataPos + Int64(FCurIntPos)
  else
    Result := 0;
end;

function TRIFFWaveDecoder.SampleTell : Integer;
var
  bpos : Int64;
begin
  bpos := RawTell;
  Result := FrameFromBytes(bpos).AsSamples;
end;

function TRIFFWaveDecoder.TimeTell : Double;
var
  bpos : Int64;
begin
  bpos := RawTell;
  Result := FrameFromBytes(bpos).AsDurationSec;
end;

function TRIFFWaveDecoder.RawTotal : Int64;
begin
  Result := FDataTotal;
end;

function TRIFFWaveDecoder.SampleTotal : Integer;
var
  bpos : Int64;
begin
  bpos := RawTotal;
  Result := FrameFromBytes(bpos).AsSamples;
end;

function TRIFFWaveDecoder.TimeTotal : Double;
var
  bpos : Int64;
begin
  bpos := RawTotal;
  Result := FrameFromBytes(bpos).AsDurationSec;
end;

{ TRawDecoder }

function TRawDecoder.ReadHeader : Boolean;
var
  aHeader : PChar;
begin
  Done;

  aHeader := GetMem(FWavFormat.HEADER_SIZE);
  try
    if DataStream.DoRead(aHeader, FWavFormat.HEADER_SIZE) = FWavFormat.HEADER_SIZE then
    begin
      Result := FWavFormat.DoReadHeader(aHeader);
    end else
      Result := false;
  finally
    FreeMem(aHeader);
  end;
end;

class function TRawDecoder.DefaultDataLimits : TSoundDataLimits;
begin
  Result := [sdpForceNotSeekable, sdpReadOnly];
end;

procedure TRawDecoder.Init;
begin
  FWavFormat := TWaveRIFFHeader.Create;
  FReady := ReadHeader;
end;

procedure TRawDecoder.Done;

procedure recursiveDeleteChunk(aChunk : pRawDataChunk);
begin
  if Assigned(aChunk^.FNext) then
    recursiveDeleteChunk(aChunk^.FNext);
  Freemem(aChunk);
end;

begin
  if Assigned(FDataChunks) then
    recursiveDeleteChunk(FDataChunks);
  FDataChunks := nil;
  FWavFormat.Free;
end;

function TRawDecoder.GetBitdepth : Cardinal;
begin
  Result := FWavFormat.GetBitdepth;
end;

function TRawDecoder.GetBitrate : Cardinal;
begin
  Result := FWavFormat.GetBitrate;
end;

function TRawDecoder.GetChannels : Cardinal;
begin
  Result := FWavFormat.GetChannels;
end;

function TRawDecoder.GetFrequency : Cardinal;
begin
  Result := FWavFormat.GetFrequency;
end;

function TRawDecoder.GetVersion : Integer;
begin
  Result := FWavFormat.GetVersion;
end;

constructor TRawDecoder.Create(aStream : TStream);
begin
  InitStream(TOGLSound.NewDataStream(aStream, DefaultDataLimits));
  Init;
end;

destructor TRawDecoder.Destroy;
begin
  Done;
  inherited Destroy;
end;

function TRawDecoder.Comments : ISoundComment;
begin
  Result := nil;
end;

function TRawDecoder.ReadData(Buffer : Pointer; Count : ISoundFrameSize;
  Par : Pointer) : ISoundFrameSize;
var
  sz, total, p : Cardinal;
  chnk : TChunkHeader;
begin
  total := Count.AsBytes;
  p := 0;
  while p < total do
  begin
    if Assigned(FDataChunks) then
    begin
      sz := Cardinal(Int64(FDataChunks^.FSize) - Int64(FCurIntPos));
    end else
      sz := 0;
    if sz = 0 then
    begin
      if Assigned(FDataChunks) then FreeAndNil(FDataChunks);
      if DataStream.DoRead(@chnk, Sizeof(TChunkHeader) ) < Sizeof(TChunkHeader) then
      begin
        Break;
      end else
      begin
        if chnk.ID = AUDIO_CHUNK_ID_data then
        begin
          FDataChunks := GetMem(Sizeof(tRawDataChunk));
          FDataChunks^.FRawPos := 0;
          FDataChunks^.FSize := chnk.Size;
          FDataChunks^.FNext := nil;
          FCurIntPos := 0;
        end else
        begin
          Result := TOGLSound.NewErrorFrame;
          Exit;
        end;
      end;
    end else
    begin
      if sz > (total - p) then sz := total - p;
      sz := DataStream.DoRead(@(PByte(Buffer)[p]), sz);
      Inc(p, sz);
      Inc(FCurIntPos, sz);
    end;
  end;
  Result := Count.EmptyDuplicate;
  Result.IncBytes(p);
end;

procedure TRawDecoder.SetStream(aStream : TStream);
begin
  (DataStream as TSoundDataStream).Stream := aStream;
end;

function TRawDecoder.Ready : Boolean;
begin
  Result := FReady;
end;

{ TRIFFWaveFile }

function TRIFFWaveFile.InitEncoder(aProps : ISoundEncoderProps;
  aComments : ISoundComment) : ISoundEncoder;
begin
  Result := TWAVE.NewStreamEncoder(Stream, DataLimits, aProps,
                                         aComments as ISoundComment);
end;

function TRIFFWaveFile.InitDecoder : ISoundDecoder;
begin
  Result := TWAVE.NewStreamDecoder(Stream, DataLimits);
end;

{ TRIFFWaveEncoder }

procedure TRIFFWaveEncoder.Init(aProps : ISoundEncoderProps;
  aComment : ISoundComment);
begin
  inherited Init(aProps, aComment);
  if Assigned(aComment) then
    FComments := aComment else
    FComments := TWAVE.NewComment;
  FChunkSize := 0;
  FLastDataChunkPos := -1;
end;

procedure TRIFFWaveEncoder.DoWriteDataChunkSize;
var
  DataChunk: TChunkHeader;
begin
  if (FLastDataChunkPos >= 0) then
  begin
    DataStream.DoSeek(FLastDataChunkPos, 0);
    with DataChunk do begin
      Id := AUDIO_CHUNK_ID_data;
      Size := FChunkSize;
    end;
    DataStream.DoWrite(@DataChunk, SizeOf(DataChunk));
  end;
end;

constructor TRIFFWaveEncoder.Create(aStream : TStream;
  aProps : ISoundEncoderProps; aComments : ISoundComment);
begin
  InitStream(TOGLSound.NewDataStream(aStream, [sdpWriteOnly]));
  Init(aProps, aComments);
end;

function TRIFFWaveEncoder.Comments : ISoundComment;
begin
  Result := FComments;
end;

function TRIFFWaveEncoder.WriteData(Buffer : Pointer; Count : ISoundFrameSize;
  Par : Pointer) : ISoundFrameSize;
var
  FrameSize, Cnt : Integer;
begin
  FrameSize := Count.AsBytes;

  if FLastDataChunkPos < 0 then
  begin
    FLastDataChunkPos := DataStream.DoTell;
    DoWriteDataChunkSize;
  end;

  Cnt := DataStream.DoWrite(Buffer, FrameSize);
  if Cnt = 0 then
  begin
    Result := Count.Duplicate;
    if Result.IsValid then
      Inc(FChunkSize, Result.AsBytes);
  end else
  begin
    Result := TOGLSound.NewErrorFrame;
  end;
end;

procedure TRIFFWaveEncoder.WriteHeader(Par : Pointer);
var
  P, Sz, OldSz : Int64;
  aHeader : PChar;
begin
  P := DataStream.DoTell;
  OldSz := DataStream.Size;
  //
  Sz := OldSz - SizeOf(TWaveFormat.ChunkHeader);
  if Sz < 0 then Sz := 0;
  DataStream.DoSeek(0, 0);

  aHeader := GetMem(FWavFormat.HEADER_SIZE);
  try
    FWavFormat.DoWriteHeader(aHeader, Sz);
    DataStream.DoWrite(aHeader, FWavFormat.HEADER_SIZE);
  finally
    FreeMem(aHeader);
  end;
  if DataStream.DoTell < P then
    DataStream.DoSeek(P, 0);
end;

procedure TRIFFWaveEncoder.Close(Par : Pointer);
begin
  Flush(Par);
  if Assigned(FComments) then
    (FComments as TRIFFComment).WriteToDataStream(DataStream);
end;

procedure TRIFFWaveEncoder.Flush(Par : Pointer);
var
  P, Sz, OldSz : Int64;
begin
  P := DataStream.DoTell;
  OldSz := DataStream.Size;
  //
  Sz := OldSz - SizeOf(TWaveFormat.ChunkHeader);
  if Sz < 0 then Sz := 0;
  DataStream.DoSeek(0, 0);
  DoWriteRIFFHeader(Sz);
  DoWriteDataChunkSize;
  if DataStream.DoTell < P then
    DataStream.DoSeek(P, 0);
  FChunkSize := 0;
  FLastDataChunkPos := -1;
end;

{ TRawEncoder }

function TRawEncoder.GetMode : TSoundEncoderMode;
begin
  Result := oemCBR;
end;

function TRawEncoder.GetQuality : Single;
begin
  Result := 0.0;
end;

procedure TRawEncoder.Init(aProps : ISoundEncoderProps; aComment : ISoundComment
  );
begin
  FWavFormat := TWaveRIFFHeader.Create;
  FWavFormat.Init(aProps);
end;

function TRawEncoder.GetBitrate : Cardinal;
begin
  Result := FWavFormat.GetBitrate;
end;

function TRawEncoder.GetChannels : Cardinal;
begin
  Result := FWavFormat.GetChannels;
end;

function TRawEncoder.GetFrequency : Cardinal;
begin
  Result := FWavFormat.GetFrequency;
end;

function TRawEncoder.GetBitdepth : Cardinal;
begin
  Result := FWavFormat.GetBitdepth;
end;

function TRawEncoder.GetVersion : Integer;
begin
  Result := FWavFormat.GetVersion;
end;

procedure TRawEncoder.DoWriteRIFFHeader(Sz : Int64);
var aHeader : Pointer;
begin
  aHeader := GetMem(FWavFormat.HEADER_SIZE);
  try
    FWavFormat.DoWriteHeader(aHeader, Sz);
    DataStream.DoWrite(aHeader, FWavFormat.HEADER_SIZE);
  finally
    FreeMem(aHeader);
  end;
end;

procedure TRawEncoder.Done;
begin
  FWavFormat.Free;
end;

constructor TRawEncoder.Create(aStream : TStream; aProps : ISoundEncoderProps);
begin
  InitStream(TOGLSound.NewDataStream(aStream, [sdpForceNotSeekable, sdpWriteOnly]));
  Init(aProps, nil);
end;

destructor TRawEncoder.Destroy;
begin
  Done;
  inherited Destroy;
end;

function TRawEncoder.Comments : ISoundComment;
begin
  Result := nil;
end;

function TRawEncoder.Ready : Boolean;
begin
  Result := true;
end;

function TRawEncoder.WriteData(Buffer : Pointer; Count : ISoundFrameSize;
  Par : Pointer) : ISoundFrameSize;
var
  DataChunk: TChunkHeader;
  FrameSize, Cnt : Integer;
  Flag : Boolean;
begin
  FrameSize := Count.AsBytes;
  with DataChunk do begin
    Id := AUDIO_CHUNK_ID_data;
    Size := FrameSize;
  end;
  Flag := DataStream.DoWrite(@DataChunk, SizeOf(DataChunk)) = SizeOf(DataChunk);
  if Flag then
  begin
    Cnt := DataStream.DoWrite(Buffer, FrameSize);
    if Cnt = FrameSize then
    begin
      Result := Count.Duplicate;
    end else
    begin
      Result := TOGLSound.NewErrorFrame;
    end;
  end else
    Result := TOGLSound.NewErrorFrame;
end;

procedure TRawEncoder.WriteHeader(Par : Pointer);
begin
  DoWriteRIFFHeader(0);
end;

procedure TRawEncoder.SetStream(aStream : TStream);
begin
  (DataStream as TSoundDataStream).Stream := aStream;
end;

{ TRIFFComment }

function TRIFFComment.WriteToMemory(Stream : TCustomMemoryStream) : Integer;
const fin : Byte = 0;
var
  chnk : TChunkHeader;
  S : RawByteString;
  i, j, c : integer;
begin
  chnk.Size := 0;
  Stream.Write(chnk, Sizeof(TChunkHeader));

  with TOGLSoundComments do
  begin
    for i := 0 to High(cCommPairs) do
    begin
      c := QueryCount(TagID(cCommPairs[i].VorbisID));
      chnk.ID := cCommPairs[i].ID;
      for j := 0 to c-1 do
      begin
        s := RawByteString(Query(TagID(cCommPairs[i].VorbisID), j));
        if length(s) > 0 then
        begin
          chnk.Size := length(s) + 1;
          Stream.Write(chnk, Sizeof(TChunkHeader));
          Stream.Write(s[1], length(s));
          Stream.Write(fin, 1);
        end;
      end;
    end;
  end;
  Stream.Position := 0;
  chnk.ID := AUDIO_CHUNK_ID_LIST;
  chnk.Size := Stream.Size - Sizeof(TChunkHeader);
  Stream.Write(chnk, Sizeof(TChunkHeader));
  Result := Stream.Size;
end;

function TRIFFComment.WriteToDataStream(Stream : ISoundDataStream) : Integer;
var
  mem : TMemoryStream;
begin
  mem := TMemoryStream.Create;
  try
    Result := WriteToMemory(mem);
    Stream.DoWrite(mem.Memory, mem.Size);
  finally
    mem.Free;
  end;
end;

function TRIFFComment.ReadFromMemory(Stream : TCustomMemoryStream) : Integer;
var
  DS : ISoundDataStream;
begin
  DS := TOGLSound.NewDataStream(Stream, [sdpForceNotSeekable, sdpReadOnly]);
  Result := ReadFromDataStream(DS);
end;

function TRIFFComment.ReadFromDataStream(Stream : ISoundDataStream) : Integer;
var
  chnk : TChunkHeader;
  total : Cardinal;
  S : PChar;
  i : integer;
begin
  Stream.DoRead(@chnk, SizeOf(chnk));
  if chnk.ID = AUDIO_CHUNK_ID_LIST then
  begin
    Result := SizeOf(chnk);
    total := chnk.Size;
    while total > 0 do
    begin
      Stream.DoRead(@chnk, SizeOf(chnk));
      Inc(Result, chnk.Size);
      if chnk.Size > 0 then
      begin
        S := GetMem(chnk.Size + 1);
        try
          Stream.DoRead(S, chnk.Size);
          Inc(Result, chnk.Size);
          S[chnk.Size] := #0;
          with TOGLSoundComments do
          begin
            for i := 0 to High(cCommPairs) do
            if cCommPairs[i].ID = chnk.ID then
              AddTag(TagID(cCommPairs[i].VorbisID), StrPas(S));
          end;
        finally
          FreeMemAndNil(S);
        end;
      end;
      Dec(total, chnk.Size + Sizeof(chnk));
    end;
  end else
    Result := 0;
end;

{ TWAVE }

class function TWAVE.NewComment : ISoundComment;
begin
  Result := TRIFFComment.Create as ISoundComment;
end;

class function TWAVE.NewComment(aSrc : ISoundComment) : ISoundComment;
begin
  Result := TRIFFComment.CreateFromInterface(aSrc) as ISoundComment;
end;

class function TWAVE.NewOggStreamEncoder(aStream : TStream;
  aDataLimits : TSoundDataLimits; aProps : ISoundEncoderProps;
  aComments : ISoundComment) : ISoundStreamEncoder;
begin
  Result := TWavOggEncoder.Create(aStream, aDataLimits, aProps, aComments) as ISoundStreamEncoder;
end;

class function TWAVE.NewOggStreamDecoder(aStream : TStream;
  aDataLimits : TSoundDataLimits) : ISoundStreamDecoder;
begin
  Result := TWavOggDecoder.Create(aStream, aDataLimits) as ISoundStreamDecoder;
end;

class function TWAVE.NewStreamEncoder(aStream : TStream;
  aDataLimits : TSoundDataLimits; aProps : ISoundEncoderProps;
  aComments : ISoundComment) : ISoundStreamEncoder;
begin
  if sdpForceNotSeekable in aDataLimits then
  begin
    Result := TRawEncoder.Create(aStream, aProps) as ISoundStreamEncoder;
  end else
  begin
    Result := TRIFFWaveEncoder.Create(aStream, aProps, aComments) as ISoundStreamEncoder;
  end;
end;

class function TWAVE.NewStreamDecoder(aStream : TStream;
  aDataLimits : TSoundDataLimits) : ISoundStreamDecoder;
begin
  if sdpForceNotSeekable in aDataLimits then
  begin
    Result := TRawDecoder.Create(aStream) as ISoundStreamDecoder;
  end else
  begin
    Result := TRIFFWaveDecoder.Create(aStream) as ISoundStreamDecoder;
  end;
end;

class function TWAVE.EncoderVersionString : String;
begin
  Result := 'raw/RIFF/Wave codec-1.0'
end;

end.

