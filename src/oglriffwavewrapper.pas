unit OGLRIFFWaveWrapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OGLSoundUtils, OGLSoundUtilTypes, fpwavformat;

type
  IRIFFComment = interface(IVorbisComment)
  ['{073ABEF2-FAC3-43FD-A90A-105F8DB86499}']
  function WriteToStream(Stream : ISoundDataStream) : Integer;
  function ReadFromStream(Stream : ISoundDataStream) : Integer;
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
    function WriteToStream(Stream : ISoundDataStream) : Integer;
    function ReadFromStream(Stream : ISoundDataStream) : Integer;
  end;

  { TRawEncoder }

  TRawEncoder = class(TSoundAbstractEncoder, ISoundStreamEncoder)
  private
    FWavFormat : TWaveFormat;
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
    function DoWriteHeader(aDataSize : Int64) : Boolean;
    procedure Done; override;
  public
    constructor Create(aStream : TStream;
                       aProps : ISoundEncoderProps);

    function Comments : ISoundComment; override;

    function Ready : Boolean; override;
    function  WriteData(Buffer : Pointer;
                   Count : ISoundFrameSize;
                   {%H-}Par : Pointer) : ISoundFrameSize; override;
    procedure WriteHeader({%H-}Par : Pointer); override;

    procedure SetStream(aStream : TStream);
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
    FWavFormat : TWaveFormat;
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

  { TRIFFWaveFile }

  TRIFFWaveFile = class(TSoundFile)
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
        (FComments as TRIFFComment).ReadFromStream(DataStream)
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
  riff: TRiffHeader;
begin
  Done;
  if DataStream.DoRead(@riff, sizeof(riff)) = sizeof(riff) then
  begin
    riff.ChunkHeader.Size := LEtoN(riff.ChunkHeader.Size);

    if (riff.ChunkHeader.ID = AUDIO_CHUNK_ID_RIFF) and (riff.Format = AUDIO_CHUNK_ID_WAVE) then
    begin
      if (DataStream.DoRead(@FWavFormat, sizeof(FWavFormat)) = sizeof(FWavFormat)) then
      begin
        LEtoN(FWavFormat);
        if (FWavFormat.ChunkHeader.ID = AUDIO_CHUNK_ID_fmt) and
           ((FWavFormat.ChunkHeader.Size + 8) >= sizeof(FWavFormat)) then
        begin
          Result := true;
        end else
          Result := false;
      end else
        Result := false;
    end else
      Result := false;
  end else
    Result := false;
end;

class function TRawDecoder.DefaultDataLimits : TSoundDataLimits;
begin
  Result := [sdpForceNotSeekable, sdpReadOnly];
end;

procedure TRawDecoder.Init;
begin
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
end;

function TRawDecoder.GetBitdepth : Cardinal;
begin
  Result := FWavFormat.BitsPerSample;
end;

function TRawDecoder.GetBitrate : Cardinal;
begin
  Result := FWavFormat.ByteRate * 8;
end;

function TRawDecoder.GetChannels : Cardinal;
begin
  Result := FWavFormat.Channels;
end;

function TRawDecoder.GetFrequency : Cardinal;
begin
  Result := FWavFormat.SampleRate;
end;

function TRawDecoder.GetVersion : Integer;
begin
  Result := RIFF_CODEC_VERSION;
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
begin
  P := DataStream.DoTell;
  OldSz := DataStream.Size;
  //
  Sz := OldSz - SizeOf(TWaveFormat.ChunkHeader);
  if Sz < 0 then Sz := 0;
  DataStream.DoSeek(0, 0);
  DoWriteHeader(Sz);
  if DataStream.DoTell < P then
    DataStream.DoSeek(P, 0);
end;

procedure TRIFFWaveEncoder.Close(Par : Pointer);
begin
  Flush(Par);
  if Assigned(FComments) then
    (FComments as TRIFFComment).WriteToStream(DataStream);
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
  DoWriteHeader(Sz);
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
  FWavFormat.ChunkHeader.ID := AUDIO_CHUNK_ID_fmt;
  FWavFormat.ChunkHeader.Size := SizeOf(FWavFormat) - SizeOf(TChunkHeader);
  FWavFormat.Format := WAVE_FORMAT_PCM;
  FWavFormat.Channels := aProps.Channels;
  FWavFormat.SampleRate := aProps.Frequency;
  FWavFormat.BitsPerSample := TOGLSound.SampleSizeToBitdepth(aProps.SampleSize);
  FWavFormat.ByteRate := (FWavFormat.SampleRate * FWavFormat.BitsPerSample * FWavFormat.Channels) div 8;
  FWavFormat.BlockAlign := (FWavFormat.BitsPerSample * FWavFormat.Channels) div 8;
end;

function TRawEncoder.GetBitrate : Cardinal;
begin
  Result := FWavFormat.SampleRate * GetBitdepth * GetChannels;
end;

function TRawEncoder.GetChannels : Cardinal;
begin
  Result := FWavFormat.Channels;
end;

function TRawEncoder.GetFrequency : Cardinal;
begin
  Result := FWavFormat.SampleRate;
end;

function TRawEncoder.GetBitdepth : Cardinal;
begin
  Result := FWavFormat.BitsPerSample;
end;

function TRawEncoder.GetVersion : Integer;
begin
  Result := RIFF_CODEC_VERSION;
end;

function TRawEncoder.DoWriteHeader(aDataSize : Int64) : Boolean;
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
  DataStream.DoWrite(@riff, SizeOf(riff));
  DataStream.DoWrite(@fmtLE, sizeof(fmtLE));
  Result := true;
end;

procedure TRawEncoder.Done;
begin
  // do nothing
end;

constructor TRawEncoder.Create(aStream : TStream; aProps : ISoundEncoderProps);
begin
  InitStream(TOGLSound.NewDataStream(aStream, [sdpForceNotSeekable, sdpWriteOnly]));
  Init(aProps, nil);
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
  DoWriteHeader(0);
end;

procedure TRawEncoder.SetStream(aStream : TStream);
begin
  (DataStream as TSoundDataStream).Stream := aStream;
end;

{ TRIFFComment }

function TRIFFComment.WriteToStream(Stream : ISoundDataStream) : Integer;
const fin : Byte = 0;
var
  chnk : TChunkHeader;
  S : RawByteString;
  i, j, c : integer;
  mem : TMemoryStream;
begin
  mem := TMemoryStream.Create;
  try
    chnk.Size := 0;
    mem.Write(chnk, Sizeof(TChunkHeader));

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
            mem.Write(chnk, Sizeof(TChunkHeader));
            mem.Write(s[1], length(s));
            mem.Write(fin, 1);
          end;
        end;
      end;
    end;
    mem.Position := 0;
    chnk.ID := AUDIO_CHUNK_ID_LIST;
    chnk.Size := mem.Size - Sizeof(TChunkHeader);
    mem.Write(chnk, Sizeof(TChunkHeader));
    Stream.DoWrite(mem.Memory, mem.Size);
    Result := mem.Size;
  finally
    mem.Free;
  end;
end;

function TRIFFComment.ReadFromStream(Stream : ISoundDataStream) : Integer;
var
  chnk : TChunkHeader;
  total : Cardinal;
  S : PChar;
  i : integer;
begin
  Stream.DoRead(@chnk, SizeOf(chnk));
  if chnk.ID = AUDIO_CHUNK_ID_LIST then
  begin
    total := chnk.Size;
    while total > 0 do
    begin
      Stream.DoRead(@chnk, SizeOf(chnk));
      if chnk.Size > 0 then
      begin
        S := GetMem(chnk.Size + 1);
        try
          Stream.DoRead(S, chnk.Size);
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
  Result := nil;
end;

class function TWAVE.NewOggStreamDecoder(aStream : TStream;
  aDataLimits : TSoundDataLimits) : ISoundStreamDecoder;
begin
  Result := nil;
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

