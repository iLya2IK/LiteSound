{
  OGLSoundLite - part of LiteSound_iLya2IK

   Copyright (c) 2023 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit OGLSoundLite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpJSON,
  ECommonObjs, OGLFastList, OGLFastNumList,
  extmemorystream,
  OGLOGGWrapper, OGLOpusWrapper, OGLFLACWrapper, OGLVorbisWrapper,
  OGLRIFFWaveWrapper,
  OGLSoundUtils, OGLSoundUtilTypes, OGLSoundDataConverting,
  OGLOpenALWrapper;

type
  // supported for now
  TSoundLiteComponent = (slcOpenAL, slcOGG, slcFLAC, slcOpus, slcVorbis);
  TSoundLiteComponents = set of TSoundLiteComponent;

  TSoundLiteCodecType = Cardinal;

  TSLPlayerState = (slsInvalid, slsInitial, slsPlaying, slsPaused, slsStopped);
  TSLCaptureState = (slcsInvalid, slcsWaiting, slcsCapturing);

  TSLCustomPlayer = class;
  TSLPlayList = class;

  { TSLAudioFrame }

  TSLAudioFrame = class(TExtMemoryStream)
  private
    FFrame : ISoundFrameSize;
    FId    : QWord;
    procedure SetFrameSize(AValue : ISoundFrameSize);
  public
    constructor Create(aID : QWord); overload;
    property FrameSize : ISoundFrameSize read FFrame write SetFrameSize;
    property FrameID : QWord read FId;
  end;

  { TSLEncodedFrame }

  TSLEncodedFrame = class(TSLAudioFrame)
  private
    FCodec : TSoundLiteCodecType;
  public
    constructor Create(aID : QWord;
                       aCodec : TSoundLiteCodecType); overload;
    property Codec : TSoundLiteCodecType read FCodec;
  end;

  TSLAudioFrames = class(specialize TThreadSafeFastBaseSeq<TSLAudioFrame>);
  TSLEncodedFrames = class(specialize TThreadSafeFastBaseSeq<TSLEncodedFrame>);

  { TSLFramedEncoder }

  TSLFramedEncoder = class(TThreadSafeObject)
  private
    FEncCodec      : TSoundLiteCodecType;
    FEncProps      : ISoundEncoderProps;
    FEncComments   : ISoundComment;
    FEncoder       : ISoundStreamEncoder;
    FCurFrameID    : TThreadQWord;
    FEncodedFrames : TSLEncodedFrames;
    FCurEncFrame   : TSLEncodedFrame;
    FTotalDuration : ISoundFrameSize;
    FMaxFrameSize  : ISoundFrameSize;
    FCurFrameSize  : ISoundFrameSize;
    function GetCurFrameID : QWord;
    function GetMaxFrameSize : ISoundFrameSize;
    function GetTotalDuration : ISoundFrameSize;
    procedure PushNext;
    procedure PushFrame;
    procedure Init(aEncCodec : TSoundLiteCodecType;
                                 aEncProps : ISoundEncoderProps;
                                 aEncComm : ISoundComment);
    procedure RestartEncoder;
    procedure SetMaxFrameSize(AValue : ISoundFrameSize);
  public
    constructor Create(aEncCodec : TSoundLiteCodecType;
                                 aEncProps : ISoundEncoderProps);  overload;
    constructor Create(aEncCodec : TSoundLiteCodecType;
                                 aEncProps : ISoundEncoderProps;
                                 aEncComm : ISoundComment);  overload;
    destructor Destroy; override;

    function VorbisOggEncoder : TVorbisOggStreamEncoder;
    function OpusOggEncoder : TOpusOggStreamEncoder;
    function OpusEncoder : TOpusStreamEncoder;
    function FLACOggEncoder : TFLACOggStreamEncoder;
    function FLACEncoder : TFLACStreamEncoder;

    function  WriteData(Buffer : Pointer;
                   Count : ISoundFrameSize;
                   Par : Pointer) : ISoundFrameSize;
    procedure Close(Par : Pointer);
    procedure Flush(Par : Pointer);

    function Ready : Boolean;

    property EncodedFrames : TSLEncodedFrames read FEncodedFrames;
    property CurrentFrameID : QWord read GetCurFRameID;
    property TotalDuration : ISoundFrameSize read GetTotalDuration;
    property MaxFrameSize : ISoundFrameSize read GetMaxFrameSize write SetMaxFrameSize;
  end;

  { TSLFramedDecoder }

  TSLFramedDecoder = class(TThreadSafeObject)
  private
    FDecCodec      : TSoundLiteCodecType;
    FDecProps      : ISoundProps;
    FDecoder       : ISoundStreamDecoder;
    FCurFrameID    : TThreadQWord;
    FDecodedFrames : TSLAudioFrames;
    FCurDecFrame   : TSLAudioFrame;
    FTotalDuration : ISoundFrameSize;
    FCurFrameSize  : ISoundFrameSize;
    FBufferSize    : ISoundFrameSize;
    FDataSource    : TStream;
    FBuffer        : Pointer;
    function  GetCurFrameID : QWord;
    function GetDataSource : TStream;
    function  GetTotalDuration : ISoundFrameSize;
    procedure PushFrame;
    procedure RestartDecoder;
    procedure SetDataSource(AValue : TStream);
  public
    constructor Create(aDecCodec : TSoundLiteCodecType;
                       aDecProps : ISoundProps); overload;
    destructor Destroy; override;

    function VorbisOggDecoder : TVorbisOggStreamAltDecoder;
    function OpusOggDecoder : TOpusOggStreamDecoder;
    function OpusDecoder : TOpusStreamDecoder;
    function FLACOggDecoder : TFLACOggStreamDecoder;
    function FLACDecoder : TFLACStreamDecoder;

    function  DecodeData(Count : ISoundFrameSize; Par : Pointer) : ISoundFrameSize;
    function  DecodeAllData(Par : Pointer) : ISoundFrameSize;
    procedure Flush;

    function Ready : Boolean;

    property DataSource : TStream read GetDataSource write SetDataSource;
    property DecodedFrames : TSLAudioFrames read FDecodedFrames;
    property CurrentFrameID : QWord read GetCurFRameID;
    property TotalDuration : ISoundFrameSize read GetTotalDuration;
  end;

  { TSLFramedDataSource }

  TSLFramedDataSource = class(TOALStreamDataSource)
  private
    FInputFrames         : TThreadSafeFastSeq;
    FCurFrame            : TSLAudioFrame;
    FFramedDecoder       : TSLFramedDecoder;
    FMaxFrameBufferingMs : Integer;
    FMaxFrameBufferingStarveMs : Integer;
    FMaxFramesCount : Integer;
    FAccumDuration  : ISoundFrameSize;
    procedure SetMaxFrameBufferingMs(AValue : Integer);
    procedure SetMaxFrameBufferingStarveMs(AValue : Integer);
  public
    constructor Create(aCodec : TSoundLiteCodecType; aProps : ISoundProps);
      overload;
    procedure  Init(aCodec : TSoundLiteCodecType; aProps : ISoundProps);
    destructor Destroy; override;

    function LoadFromFile(const {%H-}Fn : String) : Boolean; override;
    function LoadFromStream({%H-}Str : TStream) : Boolean; override;

    procedure Process;
    procedure PushFrame(aFrame : TStream);
    function  Empty : Boolean;
    function  Cached : Boolean;
    function  Starving : Boolean;

    function ReadChunk(const Buffer : Pointer;
                         {%H-}Pos : Int64;
                         Sz  : Integer;
                         {%H-}isloop : Boolean;
                         var fmt : TOALFormat;
                         var freq : Cardinal) : Integer; override;

    property MaxFramesCount : Integer read FMaxFramesCount write FMaxFramesCount;
    property MaxFrameBufferingMs : Integer read FMaxFrameBufferingMs write SetMaxFrameBufferingMs;
    property MaxFrameBufferingStarveMs : Integer read FMaxFrameBufferingStarveMs write SetMaxFrameBufferingStarveMs;
  end;

  { TSoundFileInfo }

  TSoundFileInfo = class
  private
    FCodecType  : TSoundLiteCodecType;
    FFileName   : String;
  public
    constructor Create(aSrc : TSoundFileInfo); overload;
    constructor Create(aCodecType : TSoundLiteCodecType;
                                        const aFileName : String);
                                        overload; virtual;

    function SaveToJSON : TJSONObject; virtual;

    function Comments : ISoundComment; virtual; abstract;
    function Stream : TStream; virtual; abstract;
    function Codec  : ISoundEncDec; virtual; abstract;
    function CodecType : TSoundLiteCodecType; virtual;
    function FileName  : String; virtual;

    function Frequency    : Cardinal; virtual; abstract;
    function Bitrate      : Cardinal; virtual; abstract;
    function Bitdepth     : Cardinal; virtual; abstract;
    function SampleSize   : TSoundSampleSize; virtual; abstract;
    function Channels     : Cardinal; virtual; abstract;
    function Version      : Cardinal; virtual; abstract;
    function TimeTotal    : Double;   virtual; abstract;
    function SamplesTotal : Integer;  virtual; abstract;
  end;

  { TErroredSoundFileInfo }

  TErroredSoundFileInfo = class(TSoundFileInfo)
  public
    function Comments : ISoundComment; override;
    function Stream : TStream; override;
    function Codec  : ISoundEncDec; override;

    function SaveToJSON : TJSONObject; override;

    function Frequency    : Cardinal; override;
    function Bitrate      : Cardinal; override;
    function Bitdepth     : Cardinal; override;
    function SampleSize   : TSoundSampleSize; override;
    function Channels     : Cardinal; override;
    function Version      : Cardinal; override;
    function TimeTotal    : Double;  override;
    function SamplesTotal : Integer;  override;
  end;

  { TPassiveSoundFileInfo }

  TPassiveSoundFileInfo = class(TSoundFileInfo)
  private
    FComments   : ISoundComment;
    FFreq       : Cardinal;
    FChannels   : Cardinal;
    FBitrate    : Cardinal;
    FVersion    : Cardinal;
    FSampleSize : TSoundSampleSize;
    FTotalSize  : Double;
  protected
    procedure SetComments(aComments : ISoundComment);
    procedure SetProps(aFreq, aChannels, aBitrate, aVersion : Cardinal;
                       aTotalSize : Double;
                       aSampleSize : TSoundSampleSize);
  public
    constructor Create(aSrc : TJSONObject); overload;
    destructor Destroy; override;

    function Comments : ISoundComment; override;
    function Stream : TStream; override;
    function Codec  : ISoundEncDec; override;

    function Frequency    : Cardinal; override;
    function Bitrate      : Cardinal; override;
    function Bitdepth     : Cardinal; override;
    function SampleSize   : TSoundSampleSize; override;
    function Channels     : Cardinal; override;
    function Version      : Cardinal; override;
    function TimeTotal    : Double;  override;
    function SamplesTotal : Integer;  override;
  end;

  { TPassiveSoundStreamInfo }

  TPassiveSoundStreamInfo = class(TPassiveSoundFileInfo)
  private
    FStream : TStream;
  protected
    procedure SetStream(aStr : TStream);
    function DetachStream : TStream;
  public
    destructor Destroy; override;

    property Stream : TStream read FStream;
  end;

  { TActiveSoundFileInfo }

  TActiveSoundFileInfo = class(TSoundFileInfo)
  private
    FFile : TSoundFile;
  protected
    procedure SetFile(aFile : TSoundFile);
  public
    destructor Destroy; override;

    function Comments : ISoundComment; override;
    function Stream : TStream; override;
    function Codec  : ISoundEncDec; override;

    function Frequency    : Cardinal; override;
    function Bitrate      : Cardinal; override;
    function Bitdepth     : Cardinal; override;
    function SampleSize   : TSoundSampleSize; override;
    function Channels     : Cardinal; override;
    function Version      : Cardinal; override;
    function TimeTotal    : Double;  override;
    function SamplesTotal : Integer;  override;

    property SoundFile : TSoundFile read FFile write SetFile;
  end;

  { TSLFile }

  TSLFile = class
  private
    FFileInfo : TSoundFileInfo;
  protected
    procedure ClearFileInfo;
    procedure SetFileInfo(aFileInfo : TSoundFileInfo);
    class function NewErroredFileInfo(aCodecType : TSoundLiteCodecType;
                             const aFileName : String) :  TSoundFileInfo;
    class function NewErroredFileInfo(aSrc : TSoundFileInfo) :  TSoundFileInfo;
    class function GenerateFileByCodec(aCodecType : TSoundLiteCodecType) : TSoundFile;
  public
    destructor Destroy; override;

    property FileInfo : TSoundFileInfo read FFileInfo;
  end;

  { TSLTrackFile }

  TSLTrackFile = class(TSLFile)
  private
    FActive : Boolean;
    function  SoundFile : TSoundFile;
    procedure SetActive(AValue : Boolean);

    class function NewPassiveFileInfo(aJSON : TJSONObject) :  TSoundFileInfo; overload;
    class function NewPassiveStreamInfo(aCodecType : TSoundLiteCodecType;
      const aFileName : String; aStr : TStream) :  TSoundFileInfo;
    class function NewPassiveFileInfo(aCodecType : TSoundLiteCodecType;
                             const aFileName : String) :  TSoundFileInfo; overload;
    class function NewPassiveFileInfo(aActiveFI : TActiveSoundFileInfo) :  TSoundFileInfo; overload;
    class function NewActiveFileInfo(aCodecType : TSoundLiteCodecType;
                             const aFileName : String) :  TSoundFileInfo; overload;
    class function NewActiveFileInfo(aPassivFI : TPassiveSoundFileInfo) :  TSoundFileInfo; overload;
  protected
    function LoadFromJSON(aJSON : TJSONObject) : Boolean;
  public
    function LoadFromFile(const aFileName : String;
                             doActivate : Boolean = false) : Boolean; virtual;
    function LoadFromStream(Str : TStream;
                             doActivate : Boolean = false) : Boolean;
    function ReadData(Buffer : Pointer;
                      aFrameSize : ISoundFrameSize;
                      Ptr : Pointer) : ISoundFrameSize;

    procedure Activate;
    procedure StandBy;

    property Active : Boolean read FActive write SetActive;

    procedure ResetToStart;
    procedure SeekPercent(value : Single);
    procedure SeekSample(value : Integer);
    procedure SeekTime(value : Double);
    function GetDecodedSamples : Integer;
    function GetDecodedTime : Double;
    function GetTotalTime : Double;
    function GetTotalSamples : Integer;

    function TagsCount : Integer;
    function Tag(index : Integer) : String;
    function GetTagValues(const aTag : String) : String;
    function GetTagValuesVar(const aTags : Array of String) : String;
    function TrackNo : String;
    function Title   : String;
    function Artist  : String;
    function Album   : String;
    function Genre   : String;
  end;

  { TSLOutputFile }

  TSLOutputFile = class(TSLFile)
  private
    procedure InitEncoder(const aFileName : String; aCodec : TSoundLiteCodecType);
    function SoundFile : TSoundFile;
  public
    function SaveToFile(const aFileName : String;
      aCodec : TSoundLiteCodecType;
      aProps : ISoundEncoderProps;
      aComments : ISoundComment) : Boolean; virtual;
    function SaveToStream(Str : TStream;
      aCodec : TSoundLiteCodecType;
      aDataLimits : TSoundDataLimits;
      aProps : ISoundEncoderProps;
      aComments : ISoundComment) : Boolean; virtual;
    function SaveToStream(Str : TStream;
      aCodec : TSoundLiteCodecType;
      aProps : ISoundEncoderProps;
      aComments : ISoundComment) : Boolean;
    function WriteData(Buffer : Pointer;
                       aFrameSize : ISoundFrameSize;
                       Ptr : Pointer) : ISoundFrameSize;
    procedure StopStreaming;
  end;

  TSLPlaylistAction = (slplaClear, slplaAddTrack, slplaAddTracks, slplaDeleteTrack);

  TSLOnStartPlay = procedure (Sender : TSLCustomPlayer) of object;
  TSLOnPauseResume = procedure (Sender : TSLCustomPlayer) of object;
  TSLOnStopPlay = procedure (Sender : TSLCustomPlayer) of object;

  TSLOnTrackChanged = procedure (Sender : TSLPlayList; aLstTrack, aNewTrack : TSLTrackFile) of object;
  TSLOnTrackAdd = procedure (aTrack : TSLTrackFile) of object;
  TSLOnTrackDelete = procedure (aTrack : TSLTrackFile) of object;
  TSLOnTracksAdd = procedure (aCount : Integer) of object;
  TSLOnPlaylistClear = procedure () of object;

  { TSLPlaylistConsumer }

  TSLPlaylistConsumer = class
  private
    FOnClearPlaylist : TSLOnPlaylistClear;
    FOnDeleteTrack : TSLOnTrackDelete;
    FOnAddTrack  : TSLOnTrackAdd;
    FOnAddTracks : TSLOnTracksAdd;
  protected
    procedure DoClearPlayList;
    procedure DoDeleteTrack(aTrack : TSLTrackFile);
    procedure DoAddTrack(aTrack : TSLTrackFile);
    procedure DoAddTracks(aCount : Integer);
  public
    property OnClearPlaylist : TSLOnPlaylistClear read  FOnClearPlaylist write FOnClearPlaylist;
    property OnDeleteTrack : TSLOnTrackDelete read FOnDeleteTrack write FOnDeleteTrack;
    property OnAddTrack  : TSLOnTrackAdd read FOnAddTrack write FOnAddTrack;
    property OnAddTracks : TSLOnTracksAdd read FOnAddTracks write FOnAddTracks;
  end;

  { TSLPlaylistConsumers }

  TSLPlaylistConsumers = class(specialize TThreadSafeFastBaseSeq<TSLPlaylistConsumer>)
  public
    destructor Destroy; override;
  end;

  { TSLPlayList }

  TSLPlayList = class(specialize TThreadSafeFastBaseCollection<TSLTrackFile>)
  private
    FOnTrackChanged : TSLOnTrackChanged;
    FPlayOrder : TFastIntegerList;
    FConsumers : TSLPlaylistConsumers;
    FPlayRepeat, FPlayShuffle : Boolean;
    FOrderPlayPosition : Integer;


    procedure DoClear(obj : TObject);
    procedure DoAddTrack(obj : TObject; data : Pointer);
    procedure DoAddTracks(obj : TObject; data : Pointer);
    procedure DoDeleteTrack(obj : TObject; data : Pointer);
    function GetListPlayPosition : Integer;
    procedure SendToAllConsumers(act : TSLPlaylistAction;
                                     atrack : TSLTrackFile);

    function GetListTrackPos(OrderPos : Integer) : Integer;
    function GetOrderTrackPos(ListPos : Integer) : Integer;
    procedure DeleteLastFromOrder;

    procedure AddSilent(const Obj : TSLTrackFile);

    const JSON_REPEAT : String = 'repeat';
    const JSON_SHUFFLE: String = 'shuffle';
    const JSON_POS    : String = 'pos';
    const JSON_TRACKS : String = 'tracks';
    procedure SetListPlayPosition(AValue : Integer);
    procedure SetOrderPlayPosition(AValue : Integer);
    procedure SetPlayShuffle(AValue : Boolean);
    procedure DoShuffle;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadFromJSON(aJSON : TJSONObject) : Boolean;
    function LoadFromFile(const Fn : String) : Boolean;
    function LoadFromStream(Str : TStream) : Boolean;
    function SaveToJSON : TJSONObject;
    function SaveToFile(const Fn : String) : Boolean;
    function SaveToStream(Str : TStream) : Boolean;

    procedure AddConsumer(cons : TSLPlaylistConsumer);

    function  Add(const Obj : TSLTrackFile) : Integer; override;
    function  AddFromFile(const FN : String) : Integer;
    procedure Delete(Ind : integer); override;
    procedure Clear; override;
    procedure Next;
    procedure Prev;
    function CurrentTrack : TSLTrackFile;

    procedure Shuffle;

    property PlayRepeat : Boolean read FPlayRepeat write FPlayRepeat;
    property PlayShuffle : Boolean read FPlayShuffle write SetPlayShuffle;
    property PlayPosition : Integer read GetListPlayPosition write SetListPlayPosition;

    property OnTrackChanged : TSLOnTrackChanged read FOnTrackChanged write FOnTrackChanged;
  end;

  { TSLTrackDataSource }

  TSLTrackDataSource = class(TOALStreamDataSource)
  private
    FStream : TSLTrackFile;
    FOwnFile : Boolean;
  public
    constructor Create; override;
    constructor Create(aTrackFile : TSLTrackFile; aOwnFile : Boolean); overload;
    destructor Destroy; override;

    function LoadFromFile(const Fn : String) : Boolean; override;
    function LoadFromStream(Str : TStream) : Boolean; override;

    procedure SeekSample(aSample : Integer); override;
    procedure SeekTime(aTime : Double); override;
    function  TellSamples : Integer; override;
    function  TotalSamples : Integer; override;

    function ReadChunk(const Buffer : Pointer;
                         {%H-}Pos : Int64;
                         Sz  : Integer;
                         isloop : Boolean;
                         var fmt : TOALFormat;
                         var freq : Cardinal) : Integer; override;
  end;


  { TSLFileDataRecorder }

  TSLFileDataRecorder = class(TOALStreamDataRecorder)
  private
    FStream   : TSLOutputFile;
    FComments : IVorbisComment;
    FEncProps : ISoundEncoderProps;
    function GetComments : IVorbisComment;
    function GetEncProps : ISoundEncoderProps;
    procedure SetComments(AValue : IVorbisComment);
    procedure SetEncProps(AValue : ISoundEncoderProps);
    function PrepareRecorder(aCodec : TSoundLiteCodecType) : ISoundComment;
  public
    constructor Create(aFormat : TOALFormat; aFreq : Cardinal); override;
    destructor Destroy; override;
    function SaveToFile(const Fn : String) : Boolean; override;
    function SaveToStream(Str : TStream) : Boolean; override;
    function SaveToFile(const Fn : String; aCodec : TSoundLiteCodecType) : Boolean; overload;
    function SaveToStream(Str : TStream; aCodec : TSoundLiteCodecType) : Boolean; overload;

    procedure StopRecording; override;

    function WriteSamples(const Buffer : Pointer;
                          Count : Integer) : Integer; override;

    property Comments : IVorbisComment read GetComments write SetComments;
    property EncoderProps : ISoundEncoderProps read GetEncProps write SetEncProps;
  end;

  { TSLPlayListTrackDataSource }

  TSLPlayListTrackDataSource = class(TSLTrackDataSource)
  protected
    procedure AfterApplied; override;
  public
    constructor Create(aPos : Integer; aPlayList : TSLPlayList); overload;
    destructor Destroy; override;

    function LoadFromFile(const {%H-}Fn : String) : Boolean; override;
    function LoadFromStream({%H-}Str : TStream) : Boolean; override;
  end;

  TSLPlayerThread = class;

  TSLOnNextData = procedure (aBuffer : Pointer;
                                aFrame : ISoundFrameSize) of object;

  { TSLCustomPlayer }

  TSLCustomPlayer = class(TThreadSafeObject)
  private
    FOnNextData : TSLOnNextData;
    FOnPause, FOnResume : TSLOnPauseResume;
    FOnStartPlay : TSLOnStartPlay;
    FOnStopPlay : TSLOnStopPlay;

    FLastStatus : TSLPlayerState;
    FOALPlayer : TOALPlayer;  // threadsafe wrapped

    function GetGain : Single;
    procedure SetGain(AValue : Single);
    procedure SetOnNextData(AValue : TSLOnNextData);
    procedure SetOnPause(AValue : TSLOnPauseResume);
    procedure SetOnResume(AValue : TSLOnPauseResume);
    procedure SetOnStartPlay(AValue : TSLOnStartPlay);
    procedure SetOnStopPlay(AValue : TSLOnStopPlay);
    procedure InternalNextBuffer(aBuffer : Pointer;
                                  aSize : Int64;
                                  aFormat : TOALFormat;
                                  aFrequency : Cardinal);
  protected
    procedure DoStatusChanged(aNew, aOld : TSLPlayerState); virtual;
    procedure DoAfterPlayerInit; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure InitPlayer; overload;
    procedure InitPlayer(const devicename : String); overload;
    procedure InitPlayer(const devicename : String;
                           buffers, buffersize  : Integer); overload;

    procedure FullLock; virtual;
    procedure FullUnLock; virtual;

    procedure Play; virtual;
    procedure Pause; virtual;
    procedure Resume; virtual;
    procedure Stop; virtual;

    procedure SeekTime(aTime : Double); virtual;
    procedure SeekSample(aSample : Integer); virtual;
    function PlayedTime : Double; virtual;
    function PlayedSamples : Integer; virtual;
    function DecodedTime : Double; virtual;
    function DecodedSamples : Integer; virtual;
    function Status : TSLPlayerState; virtual;
    function Playing : Boolean;
    function Paused : Boolean;
    function Stopped : Boolean;

    property Gain : Single read GetGain write SetGain;

    procedure Proceed; virtual;

    property OnStartPlay : TSLOnStartPlay read FOnStartPlay write SetOnStartPlay;
    property OnStopPlay : TSLOnStopPlay read FOnStopPlay write SetOnStopPlay;
    property OnNextData : TSLOnNextData read FOnNextData write SetOnNextData;
    property OnPause : TSLOnPauseResume read FOnPause write SetOnPause;
    property OnResume : TSLOnPauseResume read FOnResume write SetOnResume;
  end;

  { TSLFramedPlayer }

  TSLFramedPlayer = class(TSLCustomPlayer)
  private
    function GetFramedDataSource : TSLFramedDataSource;
  public
    constructor Create;

    procedure InitPlayer(const devicename : String;
                         aCodec : TSoundLiteCodecType; aProps : ISoundProps); overload;
    procedure InitPlayer(const devicename : String;
                         aCodec : TSoundLiteCodecType; aProps : ISoundProps;
                           buffers, buffersize  : Integer); overload;
    procedure InitPlayer(aCodec : TSoundLiteCodecType; aProps : ISoundProps;
                         buffers, buffersize  : Integer); overload;
    procedure InitPlayer(aCodec : TSoundLiteCodecType; aProps : ISoundProps); overload;

    property  FramedSource : TSLFramedDataSource read GetFramedDataSource;
  end;

  { TSLPlayer }

  TSLPlayer = class(TSLCustomPlayer)
  private
    FPlayList      : TSLPlayList;
    FTrackFinished : Boolean;
    FConsumer      : TSLPlaylistConsumer;
  protected
    procedure DoStatusChanged(aNew, aOld : TSLPlayerState); override;
    procedure DoClearPlayList;
    procedure DoDeleteTrack(aTrack : TSLTrackFile);
  public
    constructor Create;
    destructor Destroy; override;

    class function StartThread : TSLPlayerThread;

    procedure FullLock; override;
    procedure FullUnLock; override;

    procedure Play; override;

    procedure Proceed; override;

    property Playlist : TSLPlayList read FPlayList;
  end;

  { TSLPlayerThread }

  TSLPlayerThread = class(TThread)
  private
    FPlayer : TSLPlayer;
  public
    constructor Create;
    constructor Create(const devicename : String); overload;
    constructor Create(const devicename : String;
                           buffers, buffersize  : Integer); overload;
    destructor Destroy; override;
    procedure Execute; override;

    property Player : TSLPlayer read FPlayer;
  end;

  { TSoundSpeexResampler }

  TSoundSpeexResampler = class(TSoundAbstractBufferedResampler)
  private
    FResample : pSpeexResamplerState;
    FReady : Boolean;
  protected
    function  Init(aOutBufferSize : ISoundFrameSize;
                        aInRate : Cardinal; aQuality : Integer;
                        aProps : ISoundProps) : Boolean; override;
    procedure Done; override;
    function DoWrite(aInBuffer, aOutBuffer  : Pointer;
                                aSamples : Cardinal) : Cardinal; override;
    function DoWriteInterleave(aInBuffer, aOutBuffer  : Pointer;
                                aSamples : Cardinal) : Cardinal; override;
  public
    function  Flush : ISoundFrameSize; override;

    procedure SetInputRate(aRate : Cardinal); override;
    procedure SetQuality(aValue : Integer); override;

    function Ready : Boolean; override;
    function InputRate : Cardinal; override;
    function Quality : Integer; override;
  end;

  { TSoundFFT }

  TSoundFFT = class(TInterfacedObject)
  private
    FChannels, FCount : Cardinal;
    FWave, FX : ^PSingle;
    FInt : ^PInteger;
    FReady : Boolean;
  protected
    function  Init(aCount, aChannels : Cardinal) : Boolean; virtual;
    procedure Done; virtual;
  public
    constructor Create(aCount, aChannels : Cardinal);
    destructor Destroy; override;

    function ReInit(aCount, aChannels : Cardinal) : Boolean;

    function Ready : Boolean;
    function Channels : Cardinal;
    function SamplesCount : Cardinal;
    function SpectreSize : Cardinal;
  end;

  { TSoundForwardFFT }

  TSoundForwardFFT = class(TSoundFFT, ISoundForwardTransformation)
  private
    FZeroHarmonic : PSoundComplexData;
  protected
    function  Init(aCount, aChannels : Cardinal) : Boolean; override;
    procedure Done; override;
  public
    procedure Process(aBuffer : PPointer; aSampleSize : TSoundSampleSize);
    procedure ProcessInterleave(aBuffer : Pointer; aSampleSize : TSoundSampleSize);

    function OutputHarmonic(ch, n : integer) : TSoundComplexData;
    function OutputRaw : PPointer;
  end;

  { TSoundBackwardFFT }

  TSoundBackwardFFT = class(TSoundFFT, ISoundBackwardTransformation)
  public
    procedure Process(aBuffer : PPointer);
    procedure ProcessInterleave(aBuffer : PPointer);

    function Output : PPointer;
  end;

  TSLConvertMode = (cmodOneStep, cmodPull);
  TOnConvertProgress = procedure (Sender : TObject;
                       const aConverted, aTotal : ISoundFrameSize;
                       isFinish : Boolean) of object;

  { TSLConverter }

  TSLConverter = class(TThreadSafeObject)
  private
    FOnProgress : TOnConvertProgress;
    FSrc : TSLTrackFile;
    FDst : TSLOutputFile;
    FResampler : ISoundResampler;
    //
    FMode        : TSLConvertMode;
    FConverting  : Boolean;
    FBufferSize  : ISoundFrameSize;
    FTotalSize   : ISoundFrameSize;
    FConvdSize   : ISoundFrameSize;
    FBuffer      : Pointer;
    function GetConverting : Boolean;
    function GetDest : TSLOutputFile;
    function GetDstCodec : TSoundLiteCodecType;
    function GetSource : TSLTrackFile;
    function GetSrcCodec : TSoundLiteCodecType;
    function PrepareToConvert(aEncProps : ISoundEncoderProps) : Boolean;
    function InternalFullConvert : Boolean;
    function InternalNextStep : Integer;
    procedure SetOnProgress(AValue : TOnConvertProgress);
  protected
    procedure Done; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadFromFile(const aSrcFile : String) : Boolean;
    function SaveToFile(const aDstFile : String;
                         aMode : TSLConvertMode;
                         aConvertTo : TSoundLiteCodecType;
                         aEncProps : ISoundEncoderProps;
                         aResampler : ISoundResampler;
                         aComments : ISoundComment = nil) : Boolean;

    function DoConvert(const aSrcFile, aDstFile : String;
                        aMode : TSLConvertMode;
                        aConvertTo : TSoundLiteCodecType;
                        aEncProps : ISoundEncoderProps;
                        aResampler : ISoundResampler;
                        aComments : ISoundComment) : Boolean;

    // pull mode methods
    // 0  - can continue
    // 1  - eos
    // <0 - error
    function PullStep : Integer;
    procedure PullBreak;

    property Source : TSLTrackFile read GetSource;
    property Destination : TSLOutputFile read GetDest;
    property SrcCodec : TSoundLiteCodecType read GetSrcCodec;
    property DstCodec : TSoundLiteCodecType read GetDstCodec;
    property Converting : Boolean read GetConverting;
    property OnProgress : TOnConvertProgress read FOnProgress write SetOnProgress;
  end;

  { TSLConverterThread }

  TSLConverterThread = class(TThread)
  private
    FConverter : TSLConverter;
  public
    constructor Create;
    destructor Destroy; override;

    function StartConvert(const aSrcFile, aDstFile : String;
                          aConvertTo : TSoundLiteCodecType;
                          aEncProps  : ISoundEncoderProps;
                          aResampler : ISoundResampler;
                          aComments  : ISoundComment = nil) : Boolean; overload;

    procedure Execute; override;

    property Converter : TSLConverter read FConverter;
  end;

  { TSLCapture }

  generic TSLCapture<T> = class(TThreadSafeObject)
  private
    FCapturer : TOALCapture;
    function GetStatus : TSLCaptureState;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start;
    procedure Proceed;
    procedure Pause;
    procedure Stop;

    function TotalSamplesCaptured : Integer;
    function TotalCaptured : ISoundFrameSize;

    function Recorder : T; inline;

    property Status : TSLCaptureState read GetStatus;
  end;

  { TSLFileRecorder }

  TSLFileRecorder = class(specialize TSLCapture<TSLFileDataRecorder>)
  public
    procedure Init(FEncProps : ISoundEncoderProps); overload;
    procedure Init(const devicename : String;
                          FEncProps : ISoundEncoderProps); overload;
    procedure Init(const devicename : String;
                         FEncProps : ISoundEncoderProps;
                         buffersize  : Integer); overload;

    function SaveToFile(const Fn : String;
                              aConvertTo : TSoundLiteCodecType;
                              aEncProps : ISoundEncoderProps;
                              aComments : IVorbisComment) : Boolean; overload;
    function SaveToStream(Str : TStream;
                              aConvertTo : TSoundLiteCodecType;
                              aEncProps : ISoundEncoderProps;
                              aComments : IVorbisComment) : Boolean; overload;
    function SaveToFile(const Fn : String;
                              aConvertTo : TSoundLiteCodecType) : Boolean; overload;
    function SaveToStream(Str : TStream;
                              aConvertTo : TSoundLiteCodecType) : Boolean; overload;
  end;

  { ESoundLiteException }

  ESoundLiteException = class(Exception)
  protected
    class function ErrorNo : Cardinal; virtual;
  public
    constructor Create; overload;
  end;

  { ESLErroredFileInfo }

  ESLErroredFileInfo = class(ESoundLiteException)
  protected
    class function ErrorNo : Cardinal; override;
  end;

  { ESLFileNotActivated }

  ESLFileNotActivated = class(ESoundLiteException)
  protected
    class function ErrorNo : Cardinal; override;
  end;

  { ESLCantActivateFile }

  ESLCantActivateFile = class(ESoundLiteException)
  protected
    class function ErrorNo : Cardinal; override;
  end;

  { ESLWrongCaptureSource }

  ESLWrongCaptureSource = class(ESoundLiteException)
  protected
    class function ErrorNo : Cardinal; override;
  end;

  { TSoundLite }

  TSoundLite = class
  protected
    const cDEFAULT_CHUNK_LEN_MS = 40; //ms
    const cDEFAULT_MAX_LEN_MS = 120; //ms
    class procedure GetChunkSize(aSz : ISoundFrameSize; aProps : ISoundProps);
    class procedure GetMaxFrameSize(aSz : ISoundFrameSize; aProps : ISoundProps);
  public
    class procedure SetLibPath(const aDLLPath : UTF8String;
                               aComponents : TSoundLiteComponents);
    class procedure SetLibNames(const aDLLNames : Array of String;
                                aIgnoreDefault : Boolean;
                                aComponent : TSoundLiteComponent);
    class function InitSoundLite(const aDLLPath : UTF8String;
                                  aComponents : TSoundLiteComponents) : Boolean; overload;
    class function InitSoundLite(const aDLLPath : UTF8String) : Boolean; overload;
    class function InitSoundLite(aComponents : TSoundLiteComponents) : Boolean; overload;
    class function InitSoundLite : Boolean; overload;

    class function Loaded(aComponent : TSoundLiteComponent) : Boolean; overload;
    class function Loaded(aComponents : TSoundLiteComponents) : Boolean; overload;

    class procedure DoneSoundLite;

    const CODEC_UNKNOWN      = $000000;
    const CODEC_OGG_UNKNOWN  = $000001;

    const CODEC_FLAC         = $000010;
    const CODEC_OPUS         = $000020;
    const CODEC_VORBIS       = $000040;
    const CODEC_WAV          = $000080;
    const CODEC_OGG_FLAC     = CODEC_FLAC   or CODEC_OGG_UNKNOWN;
    const CODEC_OGG_OPUS     = CODEC_OPUS   or CODEC_OGG_UNKNOWN;
    const CODEC_OGG_VORBIS   = CODEC_VORBIS or CODEC_OGG_UNKNOWN;
    const CODEC_OGG_WAV      = CODEC_WAV    or CODEC_OGG_UNKNOWN;

    const FILENAME_GENERIC : String = '*Generic*';

    const JSON_TAG        : String = 'tag';
    const JSON_TAGS       : String = 'tags';
    const JSON_VENDOR     : String = 'vendor';
    const JSON_VALUE      : String = 'value';
    const JSON_FREQUENCY  : String = 'freq';
    const JSON_BITRATE    : String = 'bitrate';
    const JSON_BITDEPTH   : String = 'bitdepth';
    const JSON_CHANNELS   : String = 'channels';
    const JSON_TIMETOTAL  : String = 'timetotal';
    const JSON_VERISON    : String = 'version';
    const JSON_COMMENTS   : String = 'comments';
    const JSON_CODEC_TYPE : String = 'codect';
    const JSON_FILE_NAME  : String = 'filename';

    // encoder/decoder/sampler properties

    // library local props
    // framed encoder/decoder
    const PROP_CHUNK_SIZE                = $10000;
    const PROP_MAX_FRAME_SIZE            = $20000;
    // Speex sampler
    const PROP_SPEEX_SAMPLER_SKIP_ZEROS  = $30000;

    // Common props
    const PROP_CHANNELS      = TOGLSound.PROP_CHANNELS;
    const PROP_FREQUENCY     = TOGLSound.PROP_FREQUENCY;
    const PROP_SAMPLE_SIZE   = TOGLSound.PROP_SAMPLE_SIZE;
    // common encoder
    const ENC_PROP_MODE          = TOGLSound.PROP_MODE;
    const ENC_PROP_BITRATE       = TOGLSound.PROP_BITRATE;
    const ENC_PROP_QUALITY       = TOGLSound.PROP_QUALITY;
    // Common sampler
    const PROP_SAMPLER_OUT_SIZE_BYTES   = TOGLSound.PROP_SAMPLER_OUT_SIZE_BYTES;
    const PROP_SAMPLER_OUT_SIZE_SAMPLES = TOGLSound.PROP_SAMPLER_OUT_SIZE_SAMPLES;
    const PROP_SAMPLER_OUT_SIZE_MS      = TOGLSound.PROP_SAMPLER_OUT_SIZE_MS;
    const PROP_SAMPLER_INPUT_RATE       = TOGLSound.PROP_SAMPLER_INPUT_RATE;
    const PROP_SAMPLER_QUALITY          = TOGLSound.PROP_SAMPLER_QUALITY;
    // Opus Encoder
    const ENC_PROP_OPUS_MAX_PACKET_DURATION_MS  = TOpus.PROP_MAX_PACKET_DURATION_MS;
    const ENC_PROP_OPUS_MAX_PACKET_SIZE         = TOpus.PROP_MAX_PACKET_SIZE;
    const ENC_PROP_OPUS_APPLICATION             = TOpus.PROP_APPLICATION;
    const ENC_PROP_OPUS_COMPLEXITY              = TOpus.PROP_COMPLEXITY;
    const ENC_PROP_OPUS_HEADER_TYPE             = TOpus.PROP_HEADER_TYPE;
    const ENC_PROP_OPUS_HEADER_CALLBACK         = TOpus.PROP_HEADER_CALLBACK;
    const ENC_PROP_OPUS_DECISION_DELAY          = TOpus.PROP_DECISION_DELAY;
    const ENC_PROP_OPUS_COMMENT_PADDING         = TOpus.PROP_COMMENT_PADDING;
    // FLAC encoder
    const ENC_PROP_FLAC_COMPR_LEVEL  = TFLAC.PROP_COMPR_LEVEL;
    const ENC_PROP_FLAC_SUBSET       = TFLAC.PROP_SUBSET;

    class function IsCodecOgg(aCodec : TSoundLiteCodecType) : Boolean;
    class function CodecName(aCodec : TSoundLiteCodecType) : String;
    class function CodecNameShrt(aCodec : TSoundLiteCodecType) : String;
    class function EncoderVersionString(aCodec : TSoundLiteCodecType) : String;
    class function GetFileExt(aCodec : TSoundLiteCodecType) : String;
    class function TryGetCodecByFileName(const FN : String) : TSoundLiteCodecType;

    class function TimeToStr(t : Double) : String;

    class function NewVorbisComment(aJSON : TJSONObject) : IVorbisComment;
    class function GetCommentJSON(aSrc : ISoundComment) : TJSONObject;
    class function NewEncoderComment(aVorbisSrc : IVorbisComment;
                                     aConvertTo : TSoundLiteCodecType) : ISoundComment;

    class function NewStreamEncoder(aCodec : TSoundLiteCodecType;
                                     aStream : TStream;
                                     aProps : ISoundEncoderProps) : ISoundStreamEncoder; overload;
    class function NewStreamEncoder(aCodec : TSoundLiteCodecType;
                                     aStream : TStream;
                                     aDataLimits : TSoundDataLimits;
                                     aProps : ISoundEncoderProps;
                                     aComments : ISoundComment) : ISoundStreamEncoder; overload;

    class function NewSpeexResampler(aOutBufferSize : ISoundFrameSize;
                                     aInRate : Cardinal;
                                     aQuality : Integer;
                                     aProps : ISoundProps = nil) : ISoundResampler; overload;
    class function NewSpeexResampler(aProps : ISoundProps) : ISoundResampler; overload;

    class function NewForwardFFT(aSampleCount, aChannels : Cardinal) : ISoundForwardTransformation;
    class function NewBackwardFFT(aSampleCount, aChannels : Cardinal) : ISoundBackwardTransformation;
  end;

const c_SOUND_LITE_VERSION         = $0070;
      c_SOUND_LITE_VERSION_STRING  = '0.7';

implementation

uses
  variants,

  jsonparser,jsonreader,

  libFLAC_dynlite, libOpus_dynlite, libOpenALsoft_dyn, libOGG_dynlite,
  libVorbis_dyn;

const cALL_SL_Components : TSoundLiteComponents = [slcOpenAL, slcOGG,
                                                   slcFLAC, slcOpus,
                                                   slcVorbis];

type

  { TLibNames }

  TLibNames = class(TStringList)
  public
    IgnoreDefaults : Boolean;
    constructor Create;
  end;

var
  vLibsLoaded : TSoundLiteComponents;
  vDLLPath : Array [TSoundLiteComponent] of UTF8String;
  vDLLNames: Array [TSoundLiteComponent] of TLibNames;

const

  ecUNKNOWN_ERROR           = 0;
  ecERRORED_FILE_INFO       = 1;
  ecFILE_NOT_ACTIVATED      = 2;
  ecCAN_NOT_ACTIVATE        = 3;
  ecWRONG_CAPTURE_SRC       = 4;
  ecMAX_ERRORS = ecWRONG_CAPTURE_SRC;

  esLISTED : Array [0..ecMAX_ERRORS] of String = (
  {ecUNKNOWN_ERROR     }     'Unknow Error.',
  {ecERRORED_FILE_INFO }     'Errored File Info. File is not loaded or malformed.',
  {ecFILE_NOT_ACTIVATED}     'File is not activated.',
  {ecCAN_NOT_ACTIVATE}       'Can''t activate this track.',
  {ecWRONG_CAPTURE_SRC}      'Incorrect capture source - must be a descendant class of TSLFileDataRecorder.');

{ TSLCustomPlayer }

function TSLCustomPlayer.GetGain : Single;
begin
  Lock;
  try
    Result := FOALPlayer.Gain;
  finally
    UnLock;
  end;
end;

procedure TSLCustomPlayer.SetGain(AValue : Single);
begin
  Lock;
  try
    FOALPlayer.Gain := AValue;
  finally
    UnLock;
  end;
end;

procedure TSLCustomPlayer.SetOnNextData(AValue : TSLOnNextData);
begin
  if FOnNextData = AValue then Exit;
  FOnNextData := AValue;
end;

procedure TSLCustomPlayer.SetOnPause(AValue : TSLOnPauseResume);
begin
  if FOnPause = AValue then Exit;
  FOnPause := AValue;
end;

procedure TSLCustomPlayer.SetOnResume(AValue : TSLOnPauseResume);
begin
  if FOnResume = AValue then Exit;
  FOnResume := AValue;
end;

procedure TSLCustomPlayer.SetOnStartPlay(AValue : TSLOnStartPlay);
begin
  if FOnStartPlay = AValue then Exit;
  FOnStartPlay := AValue;
end;

procedure TSLCustomPlayer.SetOnStopPlay(AValue : TSLOnStopPlay);
begin
  if FOnStopPlay = AValue then Exit;
  FOnStopPlay := AValue;
end;

procedure TSLCustomPlayer.InternalNextBuffer(aBuffer : Pointer;
  aSize : Int64; aFormat : TOALFormat; aFrequency : Cardinal);
begin
  if Assigned(FOnNextData) then
  begin
    FOnNextData(aBuffer, TOGLSound.FrameFromBytes(aFrequency,
                            TOpenAL.OALFormatToChannels(aFormat),
                            TOGLSound.BitdepthToSampleSize(
                                   TOpenAL.OALFormatToBitsPerSample(aFormat)),
                            aSize));
  end;
end;

procedure TSLCustomPlayer.DoStatusChanged(aNew, aOld : TSLPlayerState);
begin
  case aNew of
   slsPlaying : begin
     if aOld = slsPaused then
     begin
       if Assigned(FOnResume) then
         FOnResume(Self);
     end else
     begin
       if Assigned(FOnStartPlay) then
         FOnStartPlay(Self);
     end;
   end;
   slsPaused : begin
     if Assigned(FOnPause) then
       FOnPause(Self);
   end;
   slsStopped : begin
     if Assigned(FOnStopPlay) then
       FOnStopPlay(Self);
   end;
  end;
end;

procedure TSLCustomPlayer.DoAfterPlayerInit;
begin
  if Assigned(FOALPlayer.Stream) then
    FOALPlayer.Stream.OnNextBuffer := @InternalNextBuffer;
end;

constructor TSLCustomPlayer.Create;
begin
  inherited Create;
  FLastStatus := slsInvalid;
  FOALPlayer := TOALPlayer.Create;
end;

destructor TSLCustomPlayer.Destroy;
begin
  FOALPlayer.Free;
  inherited Destroy;
end;

procedure TSLCustomPlayer.InitPlayer;
begin
  FOALPlayer.Init;
  DoAfterPlayerInit;
end;

procedure TSLCustomPlayer.InitPlayer(const devicename : String);
begin
  FOALPlayer.Init(devicename);
  DoAfterPlayerInit;
end;

procedure TSLCustomPlayer.InitPlayer(const devicename : String; buffers,
  buffersize : Integer);
begin
  FOALPlayer.Init(devicename, buffers, buffersize);
  DoAfterPlayerInit;
end;

procedure TSLCustomPlayer.FullLock;
begin
  Lock;
end;

procedure TSLCustomPlayer.FullUnLock;
begin
  UnLock;
end;

procedure TSLCustomPlayer.Play;
begin
  FullLock;
  try
    if Status = slsPaused then
    begin
      Resume;
    end else
    begin
      FOALPlayer.Stop;
      FOALPlayer.Play;
      FLastStatus := slsPlaying;
      if Assigned(FOnStartPlay) then
        FOnStartPlay(Self);
    end;
  finally
    FullUnLock;
  end;
end;

procedure TSLCustomPlayer.Pause;
begin
  Lock;
  try
    FOALPlayer.Pause;
    FLastStatus := slsPaused;
    if Assigned(FOnPause) then
      FOnPause(Self);
  finally
    UnLock;
  end;
end;

procedure TSLCustomPlayer.Resume;
begin
  Lock;
  try
    FOALPlayer.Resume;
    FLastStatus := slsPlaying;
    if Assigned(FOnResume) then
      FOnResume(Self);
  finally
    UnLock;
  end;
end;

procedure TSLCustomPlayer.Stop;
begin
  Lock;
  try
    FOALPlayer.Stop;
    FLastStatus := slsStopped;
    if Assigned(FOnStopPlay) then
      FOnStopPlay(Self);
  finally
    UnLock;
  end;
end;

procedure TSLCustomPlayer.SeekTime(aTime : Double);
begin
  Lock;
  try
    FOALPlayer.SeekTime(aTime);
  finally
    UnLock;
  end;
end;

procedure TSLCustomPlayer.SeekSample(aSample : Integer);
begin
  Lock;
  try
    FOALPlayer.SeekSample(aSample);
  finally
    UnLock;
  end;
end;

function TSLCustomPlayer.PlayedTime : Double;
begin
  Lock;
  try
    Result := FOALPlayer.PlayedTime;
  finally
    UnLock;
  end;
end;

function TSLCustomPlayer.PlayedSamples : Integer;
begin
  Lock;
  try
    Result := FOALPlayer.PlayedSamples;
  finally
    UnLock;
  end;
end;

function TSLCustomPlayer.DecodedTime : Double;
begin
  Lock;
  try
    Result := FOALPlayer.DecodedTime;
  finally
    UnLock;
  end;
end;

function TSLCustomPlayer.DecodedSamples : Integer;
begin
  Lock;
  try
    Result := FOALPlayer.DecodedSamples;
  finally
    UnLock;
  end;
end;

function TSLCustomPlayer.Status : TSLPlayerState;
var
  aCurStatus : TSLPlayerState;
begin
  Lock;
  try
    aCurStatus := TSLPlayerState(FOALPlayer.Status);
    Result := aCurStatus;
    if aCurStatus <> FLastStatus then
    begin
      DoStatusChanged(aCurStatus, FLastStatus);
      FLastStatus := aCurStatus;
    end;
  finally
    UnLock;
  end;
end;

function TSLCustomPlayer.Playing : Boolean;
begin
  Result := Status = slsPlaying;
end;

function TSLCustomPlayer.Paused : Boolean;
begin
  Result := Status = slsPaused;
end;

function TSLCustomPlayer.Stopped : Boolean;
begin
  Result := Status = slsStopped;
end;

procedure TSLCustomPlayer.Proceed;
var
  FStatus : TSLPlayerState;
begin
  Lock;
  try
    FStatus := Status;
    if FStatus = slsPlaying then
      FOALPlayer.Stream.Proceed;
  finally
    UnLock;
  end;
end;

{ TSLPlaylistConsumer }

procedure TSLPlaylistConsumer.DoClearPlayList;
begin
  if Assigned(FOnClearPlaylist) then
    FOnClearPlaylist();
end;

procedure TSLPlaylistConsumer.DoDeleteTrack(aTrack : TSLTrackFile);
begin
  if Assigned(FOnDeleteTrack) then
    FOnDeleteTrack(aTrack);
end;

procedure TSLPlaylistConsumer.DoAddTrack(aTrack : TSLTrackFile);
begin
  if Assigned(FOnAddTrack) then
    FOnAddTrack(aTrack);
end;

procedure TSLPlaylistConsumer.DoAddTracks(aCount : Integer);
begin
  if Assigned(FOnAddTracks) then
    FOnAddTracks(aCount);
end;

{ TSoundBackwardFFT }

procedure TSoundBackwardFFT.Process(aBuffer : PPointer);
var
  i : integer;
begin
  for i := 0 to FChannels-1 do
  begin
    move(aBuffer[0]^, FX[i]^, FCount * SizeOf(Single));
    __ogg_fdrfftb(FCount, FX[i], FWave[i], FInt[i]);
  end;
end;

procedure TSoundBackwardFFT.ProcessInterleave(aBuffer : PPointer);
begin
  // not implemented yet
end;

function TSoundBackwardFFT.Output : PPointer;
begin
  Result := PPointer(FX);
end;

{ TSoundFFT }

function TSoundFFT.Init(aCount, aChannels : Cardinal) : Boolean;
var
  i : integer;
begin
  FChannels := aChannels;
  FCount := aCount;

  if aChannels = 0 then Exit(false);

  FWave := GetMem(aChannels * Sizeof(pointer));
  FX := GetMem(aChannels * Sizeof(pointer));
  FInt := GetMem(aChannels * Sizeof(pointer));

  Result := Assigned(FWave) and Assigned(FX) and Assigned(FInt);

  if Result then
  for i := 0 to aChannels-1 do
  begin
    FWave[i] := AllocMem((2 * FCount + 15) * Sizeof(Single));
    FX[i] := AllocMem((FCount) * Sizeof(Single));
    FInt[i] := AllocMem(15 * Sizeof(Integer));

    Result := Assigned(FWave[i]) and Assigned(FX[i]) and Assigned(FInt[i]);
    if not Result then Exit;

    __ogg_fdrffti(FCount, FWave[i], FInt[i]);
  end;
end;

procedure TSoundFFT.Done;
var
  i : integer;
begin
  for i := 0 to FChannels-1 do
  begin
    if Assigned(FX) and Assigned(FX[i]) then
      FreeMemAndNil(FX[i]);
    if Assigned(FInt) and Assigned(FInt[i]) then
      FreeMemAndNil(FInt[i]);
    if Assigned(FWave) and Assigned(FWave[i]) then
      FreeMemAndNil(FWave[i]);
  end;

  if Assigned(FX) then
    FreeMemAndNil(FX);
  if Assigned(FInt) then
    FreeMemAndNil(FInt);
  if Assigned(FWave) then
    FreeMemAndNil(FWave);
  FReady := false;
end;

constructor TSoundFFT.Create(aCount, aChannels : Cardinal);
begin
  inherited Create;
  FReady := Init(aCount, aChannels);
end;

destructor TSoundFFT.Destroy;
begin
  Done;
  inherited Destroy;
end;

function TSoundFFT.ReInit(aCount, aChannels : Cardinal) : Boolean;
begin
  Done;
  Result := Init(aCount, aChannels);
end;

function TSoundFFT.Ready : Boolean;
begin
  Result := FReady;
end;

function TSoundFFT.Channels : Cardinal;
begin
  Result := FChannels;
end;

function TSoundFFT.SamplesCount : Cardinal;
begin
  Result := FCount;
end;

function TSoundFFT.SpectreSize : Cardinal;
begin
  Result := FCount div 2;
end;

{ TSoundForwardFFT }

function TSoundForwardFFT.Init(aCount, aChannels : Cardinal) : Boolean;
begin
  Result := inherited Init(aCount, aChannels);
  FZeroHarmonic := GetMem(Sizeof(TSoundComplexData) * FChannels);
  FillByte(FZeroHarmonic^, Sizeof(TSoundComplexData) * FChannels, 0);
end;

procedure TSoundForwardFFT.Done;
begin
  FreeMemAndNil(FZeroHarmonic);
  inherited Done;
end;

procedure TSoundForwardFFT.Process(aBuffer : PPointer; aSampleSize : TSoundSampleSize);
var
  i : integer;
begin
  for i := 0 to FChannels-1 do
  begin
    InterleaveSamples(aBuffer[i], FX[i], aSampleSize, ssFloat, true, 1, FCount);
    __ogg_fdrfftf(FCount, FX[i], FWave[i], FInt[i]);
  end;
end;

procedure TSoundForwardFFT.ProcessInterleave(aBuffer : Pointer;
  aSampleSize : TSoundSampleSize);
var
  i : integer;
begin
  UninterleaveSamples(aBuffer, PPointer( FX ), aSampleSize, ssFloat, true, FChannels, FCount);
  for i := 0 to FChannels-1 do
  begin
    __ogg_fdrfftf(FCount, FX[i], FWave[i], FInt[i]);
    FZeroHarmonic[i].r := FX[i][0];
    FZeroHarmonic[i].i := 0;
  end;
end;

function TSoundForwardFFT.OutputHarmonic(ch, n : integer) : TSoundComplexData;
begin
  // the first value of ft is always (offset + i*0.0)
  // in this case FX[ch][0] = 0.0
  // and the complex data for the 1st harmonics begins at
  // FX[ch][1] = Output(ch).r
  if n = 0 then
  begin
    Result := FZeroHarmonic[ch];
  end else
    Result := PSoundComplexData(@(FX[ch][1 + (n-1) shl 1]))^;
end;

function TSoundForwardFFT.OutputRaw : PPointer;
begin
  Result := PPointer(FX);
end;

{ TSoundSpeexResampler }

function TSoundSpeexResampler.Init(aOutBufferSize : ISoundFrameSize;
  aInRate : Cardinal; aQuality : Integer; aProps : ISoundProps) : Boolean;
var
  err : Integer;
begin
  FReady := inherited Init(aOutBufferSize, aInRate, aQuality, aProps);
  FResample := nil;
  if FReady then
  begin
    if (OutFrameSize.SampleSize in [ss16bit, ssFloat]) then
    begin
      FResample := speex_resampler_init(aOutBufferSize.Channels,
                                     aInRate, aOutBufferSize.Frequency,
                                     aQuality, @err);
      FReady := err = 0;
      if FReady and Assigned(aProps) then
      begin
        if aProps.GetDefault(TSoundLite.PROP_SPEEX_SAMPLER_SKIP_ZEROS, false) then
          speex_resampler_skip_zeros(FResample);
      end;
    end else
      FReady := false;
  end;
  Result := FReady;
end;

procedure TSoundSpeexResampler.Done;
begin
  inherited Done;
  if assigned(FResample) then
    speex_resampler_destroy(FResample);
end;

function TSoundSpeexResampler.DoWrite(aInBuffer, aOutBuffer : Pointer;
  aSamples : Cardinal) : Cardinal;
begin
  // not supported for now. need to implement
  Result := 0;
end;

function TSoundSpeexResampler.DoWriteInterleave(aInBuffer,
  aOutBuffer : Pointer; aSamples : Cardinal) : Cardinal;
begin
  Result := OutFrameSize.AsSamples;
  if OutFrameSize.SampleSize = ss16bit then
    speex_resampler_process_interleaved_int(FResample, aInBuffer, @aSamples, aOutBuffer, @Result)
  else
  if OutFrameSize.SampleSize = ssFloat then
    speex_resampler_process_interleaved_float(FResample, aInBuffer, @aSamples, aOutBuffer, @Result)
  else
    Result := 0;
end;

function TSoundSpeexResampler.Flush : ISoundFrameSize;
var
  zeros : pointer;
  drainSamples, drain, tmp : integer;
  outsamp : Cardinal;
  buf : pointer;
begin
  drainSamples := 0;
  // drain resampler
  if OutFrameSize.SampleSize = ss16bit then
    zeros := pByte(AllocMem(100*OutFrameSize.Channels * sizeof(Int16)))
  else
  if OutFrameSize.SampleSize = ssFloat then
    zeros := pByte(AllocMem(100*OutFrameSize.Channels * sizeof(Single)))
  else
  begin
    Result := TOGLSound.NewErrorFrame;
    Exit;
  end;

  drain := speex_resampler_get_input_latency(FResample);
  repeat
     if drain < 100 then tmp := drain else tmp := 100;

     outsamp := tmp;
     if OutFrameSize.SampleSize = ss16bit then
     begin
       buf := @(Pbyte(OutBuffer)[drainSamples * OutFrameSize.Channels * sizeof(Int16)]);
       speex_resampler_process_interleaved_int(FResample, zeros, @tmp,  buf, @outsamp)
     end
     else
     begin
       buf := @(Pbyte(OutBuffer)[drainSamples * OutFrameSize.Channels * sizeof(Single)]);
       speex_resampler_process_interleaved_float(FResample, zeros, @tmp, buf, @outsamp)
     end;
     drainSamples += outsamp;
     drain-=tmp;
  until (drain <= 0);
  freemem(zeros);

  Result := OutFrameSize.EmptyDuplicate;
  if drainSamples > 0 then
    Result.IncSamples(drainSamples);
  SetBufferSize(Result.AsBytes);
end;

procedure TSoundSpeexResampler.SetInputRate(aRate : Cardinal);
begin
  if Ready then
  begin
    if speex_resampler_set_rate(FResample, aRate, OutFrameSize.Frequency) <> 0 then
      FReady := false;
  end;
end;

procedure TSoundSpeexResampler.SetQuality(aValue : Integer);
begin
  if Ready then
  begin
    if speex_resampler_set_quality(FResample, aValue) <> 0 then
      FReady := false;
  end;
end;

function TSoundSpeexResampler.Ready : Boolean;
begin
  Result := FReady;
end;

function TSoundSpeexResampler.InputRate : Cardinal;
begin
  if Ready then
   speex_resampler_get_rate(FResample, nil, @Result) else
   Result := 0;
end;

function TSoundSpeexResampler.Quality : Integer;
begin
  if Ready then
   speex_resampler_get_quality(FResample, @Result) else
   Result := 0;
end;

{ TSLFramedPlayer }

function TSLFramedPlayer.GetFramedDataSource : TSLFramedDataSource;
begin
  Result := TSLFramedDataSource(FOALPlayer.Stream.DataSource);
end;

constructor TSLFramedPlayer.Create;
begin
  inherited Create;
  FOALPlayer.DataSourceClass := TSLFramedDataSource;
end;

procedure TSLFramedPlayer.InitPlayer(const devicename : String;
  aCodec : TSoundLiteCodecType; aProps : ISoundProps);
begin
  inherited InitPlayer(devicename);
  if Assigned(FramedSource) then
  begin
    FramedSource.Init(aCodec, aProps);
  end;
end;

procedure TSLFramedPlayer.InitPlayer(const devicename : String;
  aCodec : TSoundLiteCodecType; aProps : ISoundProps; buffers,
  buffersize : Integer);
begin
  inherited InitPlayer(devicename, buffers, buffersize);
  if Assigned(FramedSource) then
  begin
    FramedSource.Init(aCodec, aProps);
  end;
end;

procedure TSLFramedPlayer.InitPlayer(aCodec : TSoundLiteCodecType;
  aProps : ISoundProps; buffers, buffersize : Integer);
begin
  inherited InitPlayer('', buffers, buffersize);
  if Assigned(FramedSource) then
  begin
    FramedSource.Init(aCodec, aProps);
  end;
end;

procedure TSLFramedPlayer.InitPlayer(aCodec : TSoundLiteCodecType;
  aProps : ISoundProps);
begin
  inherited InitPlayer;
  if Assigned(FramedSource) then
  begin
    FramedSource.Init(aCodec, aProps);
  end;
end;

{ TSLFramedDataSource }

procedure TSLFramedDataSource.SetMaxFrameBufferingMs(AValue : Integer);
begin
  if FMaxFrameBufferingMs = AValue then Exit;
  FMaxFrameBufferingMs := AValue;
end;

procedure TSLFramedDataSource.SetMaxFrameBufferingStarveMs(AValue : Integer);
begin
  if FMaxFrameBufferingStarveMs = AValue then Exit;
  FMaxFrameBufferingStarveMs := AValue;
end;

constructor TSLFramedDataSource.Create(aCodec : TSoundLiteCodecType;
                                       aProps : ISoundProps);
begin
  inherited Create;
  Init(aCodec, aProps);
end;

procedure TSLFramedDataSource.Init(aCodec : TSoundLiteCodecType;
  aProps : ISoundProps);
begin
  FInputFrames := TThreadSafeFastSeq.Create;
  FFramedDecoder := TSLFramedDecoder.Create(aCodec, aProps);
  FCurFrame := nil;
  FAccumDuration := nil;
  FMaxFrameBufferingMs := 6000;
  FMaxFrameBufferingStarveMs := 3000;
  FMaxFramesCount := 100;
end;

destructor TSLFramedDataSource.Destroy;
begin
  if Assigned(FCurFrame) then
    FreeAndNil(FCurFrame);
  FFramedDecoder.Free;
  FInputFrames.Free;
  inherited Destroy;
end;

function TSLFramedDataSource.LoadFromFile(const Fn : String) : Boolean;
begin
  // ignore
  Result := false;
end;

function TSLFramedDataSource.LoadFromStream(Str : TStream) : Boolean;
begin
  // ignore
  Result := false;
end;

procedure TSLFramedDataSource.Process;
var
  aFrame : TStream;
  aDecoded : ISoundFrameSize;
begin
  aFrame := TStream(FInputFrames.PopValue);
  if Assigned(aFrame) then
  begin
    FFramedDecoder.DataSource := aFrame;
    aDecoded := FFramedDecoder.DecodeAllData(nil);
    if aDecoded.IsValid then
    begin
      if Assigned(FAccumDuration) then
        FAccumDuration.Inc(aDecoded) else
        FAccumDuration := aDecoded;
      if FFramedDecoder.DecodedFrames.Count > MaxFramesCount then
        FFramedDecoder.DecodedFrames.Erase(FFramedDecoder.DecodedFrames.ListBegin);
    end;
  end;
end;

procedure TSLFramedDataSource.PushFrame(aFrame : TStream);
begin
  FInputFrames.Push_back(aFrame);

  if FInputFrames.Count > MaxFramesCount then
    FInputFrames.Erase(FInputFrames.ListBegin);
end;

function TSLFramedDataSource.Empty : Boolean;
begin
  Result := (FFramedDecoder.DecodedFrames.Count = 0) and (not Assigned(FCurFrame));
end;

function TSLFramedDataSource.Cached : Boolean;
begin
  if Assigned(FAccumDuration) and FAccumDuration.IsValid then
    Result := FAccumDuration.AsDurationMs >= FMaxFrameBufferingMs else
    Result := false;
end;

function TSLFramedDataSource.Starving : Boolean;
begin
  if Assigned(FAccumDuration) and FAccumDuration.IsValid then
    Result := FAccumDuration.AsDurationMs < FMaxFrameBufferingStarveMs else
    Result := true;
end;

function TSLFramedDataSource.ReadChunk(const Buffer : Pointer; Pos : Int64;
  Sz : Integer; isloop : Boolean; var fmt : TOALFormat; var freq : Cardinal
  ) : Integer;
var
  rsz, rdsz : Integer;
  nEoS : Boolean;
begin
  rsz := 0;
  nEoS := true;
  while (rsz < Sz) and (nEoS) do
  begin
    if Assigned(FCurFrame) then
    begin
      fmt := TOpenAL.OALFormat(FCurFrame.FrameSize.Channels,
                               FCurFrame.FrameSize.BitDepth);
      freq := FCurFrame.FrameSize.Frequency;

      rdsz := FCurFrame.Read(PByte(Buffer)[rsz], (Sz - rsz));
      if rdsz <= 0 then
      begin
        FAccumDuration.Dec(FCurFrame.FrameSize);
        FreeAndNil(FCurFrame);
      end else
        Inc(rsz, rdsz);
    end else
    begin
      if Empty then
        nEoS := false
      else
        FCurFrame := FFramedDecoder.DecodedFrames.PopValue;
    end;
  end;
  Result := rsz;
end;

{ TSLCapture }

function TSLCapture.GetStatus : TSLCaptureState;
begin
  Lock;
  try
    Result := TSLCaptureState(FCapturer.Status);
  finally
    UnLock;
  end;
end;

function TSLCapture.Recorder : T;
begin
  Lock;
  try
    Result := T(FCapturer.Recorder);
  finally
    UnLock;
  end;
end;

constructor TSLCapture.Create;
begin
  inherited Create;
  FCapturer := TOALCapture.Create;
  FCapturer.DataRecClass := T;
end;

destructor TSLCapture.Destroy;
begin
  FCapturer.Free;
  inherited Destroy;
end;

procedure TSLCapture.Start;
begin
  Lock;
  try
    FCapturer.Start;
  finally
    UnLock;
  end;
end;

procedure TSLCapture.Proceed;
begin
  Lock;
  try
    FCapturer.Proceed;
  finally
    UnLock;
  end;
end;

procedure TSLCapture.Pause;
begin
  Lock;
  try
    FCapturer.Pause;
  finally
    UnLock;
  end;
end;

procedure TSLCapture.Stop;
begin
  Lock;
  try
    FCapturer.Stop;
  finally
    UnLock;
  end;
end;

function TSLCapture.TotalSamplesCaptured : Integer;
begin
  Lock;
  try
    Result := FCapturer.TotalSamplesCaptured;
  finally
    UnLock;
  end;
end;

function TSLCapture.TotalCaptured : ISoundFrameSize;
begin
  Lock;
  try
    Result := TOGLSound.NewEmptyFrame(FCapturer.Frequency, FCapturer.Channels,
                                      TOGLSound.BitdepthToSampleSize(FCapturer.BitsPerSample));
    Result.IncSamples(FCapturer.TotalSamplesCaptured);
  finally
    UnLock;
  end;
end;

{ ESLWrongCaptureSource }

class function ESLWrongCaptureSource.ErrorNo : Cardinal;
begin
  Result := ecWRONG_CAPTURE_SRC;
end;

{ TSLFileRecorder }

procedure TSLFileRecorder.Init(FEncProps : ISoundEncoderProps);
begin
  Init('', FEncProps, FCapturer.DefaultMaxBufferSize);
end;

procedure TSLFileRecorder.Init(const devicename : String;
  FEncProps : ISoundEncoderProps);
begin
  Init(devicename, FEncProps, FCapturer.DefaultMaxBufferSize);
end;

procedure TSLFileRecorder.Init(const devicename : String;
  FEncProps : ISoundEncoderProps; buffersize : Integer);
var
  freq, channels : Cardinal;
  oalfmt : TOALFormat;
  ss : TSoundSampleSize;
begin
  freq := FEncProps.Frequency;
  channels := FEncProps.Channels;
  ss := FEncProps.SampleSize;

  oalfmt := TOpenAL.OALFormat(channels, TOGLSound.SampleSizeToBitdepth(ss));

  FCapturer.Init(devicename, oalfmt, freq, buffersize);
end;

function TSLFileRecorder.SaveToFile(const Fn : String;
  aConvertTo : TSoundLiteCodecType; aEncProps : ISoundEncoderProps;
  aComments : IVorbisComment) : Boolean;
begin
  if Assigned(Recorder) then
  begin
    Recorder.EncoderProps := aEncProps;
    Recorder.Comments := aComments;
    Result := Recorder.SaveToFile(Fn, aConvertTo);
  end else
    Result := false;
end;

function TSLFileRecorder.SaveToStream(Str : TStream;
  aConvertTo : TSoundLiteCodecType; aEncProps : ISoundEncoderProps;
  aComments : IVorbisComment) : Boolean;
begin
  if Assigned(Recorder) then
  begin
    Recorder.EncoderProps := aEncProps;
    Recorder.Comments := aComments;
    Result := Recorder.SaveToStream(Str, aConvertTo);
  end else
    Result := false;
end;

function TSLFileRecorder.SaveToFile(const Fn : String;
  aConvertTo : TSoundLiteCodecType) : Boolean;
begin
  if Assigned(Recorder) then
    Result := Recorder.SaveToFile(Fn, aConvertTo) else
    Result := false;
end;

function TSLFileRecorder.SaveToStream(Str : TStream;
  aConvertTo : TSoundLiteCodecType) : Boolean;
begin
  if Assigned(Recorder) then
    Result := Recorder.SaveToStream(Str, aConvertTo) else
    Result := false;
end;

{ TSLFileDataRecorder }

function TSLFileDataRecorder.GetComments : IVorbisComment;
begin
  Result := FComments;
end;

function TSLFileDataRecorder.GetEncProps : ISoundEncoderProps;
begin
  Result := FEncProps;
end;

procedure TSLFileDataRecorder.SetComments(AValue : IVorbisComment);
begin
  if AValue is IVorbisComment then
  begin
    FComments := AValue;
  end else
    FComments := TOGLSound.NewVorbisComment(FComments);
end;

procedure TSLFileDataRecorder.SetEncProps(AValue : ISoundEncoderProps);
begin
  FEncProps := AValue;
end;

function TSLFileDataRecorder.PrepareRecorder(aCodec : TSoundLiteCodecType) : ISoundComment;
var
  channels : Cardinal;
  samplesize : TSoundSampleSize;
begin
  case Format of
  oalfMono8 : begin
    samplesize := ss8bit;
    channels := 1;
    end;
  oalfMono16 : begin
    samplesize := ss16bit;
    channels := 1;
    end;
  oalfStereo8 : begin
    samplesize := ss8bit;
    channels := 2;
    end;
  oalfStereo16 : begin
    samplesize := ss16bit;
    channels := 2;
    end;
  end;

  FEncProps.Add(TOGLSound.PROP_CHANNELS, channels);
  FEncProps.Add(TOGLSound.PROP_FREQUENCY, Frequency);
  FEncProps.Add(TOGLSound.PROP_SAMPLE_SIZE, samplesize);

  Result := TSoundLite.NewEncoderComment(FComments, aCodec);
end;

constructor TSLFileDataRecorder.Create(aFormat : TOALFormat; aFreq : Cardinal);
begin
  inherited Create(aFormat, aFreq);
  FStream := TSLOutputFile.Create;
  FEncProps := TOGLSound.EncProps([]);
  FComments := TOGLSound.NewVorbisComment;
end;

destructor TSLFileDataRecorder.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

function TSLFileDataRecorder.SaveToFile(const Fn : String) : Boolean;
var
  aCodec : TSoundLiteCodecType;
begin
  aCodec :=  TSoundLite.TryGetCodecByFileName(Fn);
  if aCodec = TSoundLite.CODEC_UNKNOWN then Exit(false);
  Result := SaveToFile(Fn, aCodec);
end;

function TSLFileDataRecorder.SaveToStream({%H-}Str : TStream) : Boolean;
begin
  Result := false;
end;

function TSLFileDataRecorder.SaveToFile(const Fn : String;
  aCodec : TSoundLiteCodecType) : Boolean;
var
  nativeComments : ISoundComment;
begin
  nativeComments := PrepareRecorder(aCodec);
  Result := FStream.SaveToFile(Fn, aCodec, FEncProps, nativeComments);
end;

function TSLFileDataRecorder.SaveToStream(Str : TStream;
  aCodec : TSoundLiteCodecType) : Boolean;
var
  nativeComments : ISoundComment;
begin
  nativeComments := PrepareRecorder(aCodec);
  Result := FStream.SaveToStream(Str, aCodec, FEncProps, nativeComments);
end;

procedure TSLFileDataRecorder.StopRecording;
begin
  FStream.StopStreaming;
end;

function TSLFileDataRecorder.WriteSamples(const Buffer : Pointer;
  Count : Integer) : Integer;
begin
  Result := FStream.WriteData(Buffer,
                              FStream.FileInfo.Codec.FrameFromSamples(Count),
                              nil).AsSamples;
end;

{ TSLConverterThread }

constructor TSLConverterThread.Create;
begin
  inherited Create(True);
  FConverter := TSLConverter.Create;
  FreeOnTerminate := false;
end;

destructor TSLConverterThread.Destroy;
begin
  if Assigned(FConverter) then
    FreeAndNil(FConverter);
  inherited Destroy;
end;

function TSLConverterThread.StartConvert(const aSrcFile, aDstFile : String;
  aConvertTo : TSoundLiteCodecType; aEncProps : ISoundEncoderProps;
  aResampler : ISoundResampler; aComments : ISoundComment) : Boolean;
begin
  Result := FConverter.DoConvert(aSrcFile, aDstFile,
                                           cmodPull,  aConvertTo, aEncProps,
                                           aResampler,
                                           aComments);
  if Result then
    Start;
end;

procedure TSLConverterThread.Execute;
var
  Res : Integer;
begin
  while not Terminated do
  begin
    Res := FConverter.PullStep;
    Sleep(0);
    if Res <> 0 then Exit;
  end;
end;

{ TSLConverter }

function TSLConverter.GetDest : TSLOutputFile;
begin
  Lock;
  try
    Result := FDst;
  finally
    UnLock;
  end;
end;

function TSLConverter.GetConverting : Boolean;
begin
  Lock;
  try
    Result := FConverting;
  finally
    UnLock;
  end;
end;

function TSLConverter.GetDstCodec : TSoundLiteCodecType;
begin
  Lock;
  try
     if Assigned(FDst) then
      Result := FDst.FileInfo.CodecType else
      Result := TSoundLite.CODEC_UNKNOWN;
  finally
    UnLock;
  end;
end;

function TSLConverter.GetSource : TSLTrackFile;
begin
  Lock;
  try
    Result := FSrc;
  finally
    UnLock;
  end;
end;

function TSLConverter.GetSrcCodec : TSoundLiteCodecType;
begin
  Lock;
  try
    if Assigned(FSrc) then
      Result := FSrc.FileInfo.CodecType else
      Result := TSoundLite.CODEC_UNKNOWN;
  finally
    UnLock;
  end;
end;

function TSLConverter.PrepareToConvert(aEncProps : ISoundEncoderProps) : Boolean;
begin
  FTotalSize := FSrc.FileInfo.Codec.EmptyFrame;

  if FTotalSize.IsErrored then Exit(false);

  FConvdSize := FTotalSize.EmptyDuplicate;

  FBufferSize := FTotalSize.EmptyDuplicate;
  TSoundLite.GetChunkSize(FBufferSize, aEncProps);

  FTotalSize.IncSamples(FSrc.FileInfo.SamplesTotal);

  if FBufferSize.IsEmptyOrErrored then Exit(false);
  if Assigned(FBuffer) then FreeMemAndNil(FBuffer);
  FBuffer := GetMem(FBufferSize.AsBytes);

  FConverting := True;
  Result := True;
end;

function TSLConverter.InternalFullConvert : Boolean;
var
  Res : Integer;
begin
  repeat
    Res := InternalNextStep;
  until Res <> 0;

  if Assigned(FOnProgress) then
    FOnProgress(Self, FConvdSize, FTotalSize, true);

  Result := (Res = 1);
end;

function TSLConverter.InternalNextStep : Integer;
var
  len, rlen : ISoundFrameSize;
  wbuf : Pointer;
begin
  if not FConverting then
    Result := -1
  else
  begin
    len := FSrc.ReadData(FBuffer, FBufferSize, nil);
    if Assigned(FResampler) then
    begin
      FResampler.RequestBuffer(len.AsSamples);
      rlen := FResampler.WriteInterleave(FBuffer, len);
      wbuf := FResampler.OutBuffer;
    end else
    begin
      wbuf := FBuffer;
      rlen := len;
    end;

    if rlen.IsErrored then
      Result := -2
    else
    begin
      if rlen.IsEmpty then
      begin
        Result := 1;
        FDst.StopStreaming;
      end else
      begin
        Result := 0;
        FConvdSize.Inc(rlen);
        if FTotalSize.Less(FConvdSize) then
          FTotalSize.Assign(FConvdSize);
        if FDst.WriteData(wbuf, rlen, nil).IsEmptyOrErrored then
          Result := -2;
      end;
    end;
  end;

  if Result <> 0 then
    FConverting := false;
end;

procedure TSLConverter.SetOnProgress(AValue : TOnConvertProgress);
begin
  Lock;
  try
    if FOnProgress = AValue then Exit;
    FOnProgress := AValue;
  finally
    UnLock;
  end;
end;

procedure TSLConverter.Done;
begin
  if Assigned(FDst) then
    FreeAndNil(FDst);
  if Assigned(FSrc) then
    FreeAndNil(FSrc);
  if Assigned(FBuffer) then
    FreeMemAndNil(FBuffer);

  FBufferSize := nil;
  FTotalSize := nil;
  FConvdSize := nil;

  FConverting := false;
end;

constructor TSLConverter.Create;
begin
  inherited Create;
  FBuffer := nil;
  FBufferSize := TOGLSound.NewErrorFrame;
  FSrc := nil;
  FDst := nil;
  FBuffer := nil;
  FResampler := nil;
  FConverting := false;
  FMode := cmodOneStep;
end;

destructor TSLConverter.Destroy;
begin
  Done;
  inherited Destroy;
end;

function TSLConverter.LoadFromFile(const aSrcFile : String) : Boolean;
begin
  Lock;
  try
    if FConverting then Exit(false);

    Done;

    FSrc := TSLTrackFile.Create;
    Result := FSrc.LoadFromFile(aSrcFile, false);
  finally
    UnLock;
  end;
end;

function TSLConverter.SaveToFile(const aDstFile : String;
  aMode : TSLConvertMode; aConvertTo : TSoundLiteCodecType;
  aEncProps : ISoundEncoderProps; aResampler : ISoundResampler;
  aComments : ISoundComment) : Boolean;
var
  aVorbisComments : IVorbisComment;
begin
  Lock;
  try
    if FConverting then Exit(false);

    if not Assigned(FSrc) then Exit(false);
    if FSrc.FileInfo is TErroredSoundFileInfo then Exit(false);
    FSrc.Activate;
    if not FSrc.Active then Exit(false);

    if Assigned(aResampler) then
    begin
      FResampler := aResampler;
      if FResampler.InputRate <> FSrc.FileInfo.Frequency then
        FResampler.SetInputRate(FSrc.FileInfo.Frequency);

      if not FResampler.Ready then Exit(false);
    end;

    if Assigned(FDst) then FreeAndNil(FDst);
    FDst := TSLOutputFile.Create;

    with TSoundLite, TOGLSoundComments do
    begin
      if Assigned(aComments) then
      begin
        if aComments is IVorbisComment then
          aVorbisComments := aComments as IVorbisComment
        else
          aVorbisComments := TOGLSound.NewVorbisComment(aComments) as IVorbisComment;
      end else
        aVorbisComments := TOGLSound.NewVorbisComment(FSrc.FileInfo.Comments) as IVorbisComment;


      aComments := NewEncoderComment(aVorbisComments, aConvertTo);

      if not aEncProps.HasProp(PROP_SAMPLE_SIZE) then
        aEncProps.Add(PROP_SAMPLE_SIZE, FSrc.FileInfo.SampleSize);
      if not aEncProps.HasProp(PROP_CHANNELS) then
        aEncProps.Add(PROP_CHANNELS, FSrc.FileInfo.Channels);
      if not aEncProps.HasProp(PROP_FREQUENCY) then
        aEncProps.Add(PROP_FREQUENCY, FSrc.FileInfo.Frequency);
    end;

    Result := FDst.SaveToFile(aDstFile, aConvertTo, aEncProps, aComments);
    if Result then
    begin
      //FDst.SoundFile.Encoder.WriteHeader(nil);
      FMode := aMode;
      Result := PrepareToConvert(aEncProps);
      if Result and (FMode = cmodOneStep) then
      begin
        Result := InternalFullConvert;
      end;
    end;
  finally
    UnLock;
  end;
end;

function TSLConverter.DoConvert(const aSrcFile, aDstFile : String;
  aMode : TSLConvertMode; aConvertTo : TSoundLiteCodecType;
  aEncProps : ISoundEncoderProps; aResampler : ISoundResampler;
  aComments : ISoundComment) : Boolean;
begin
  Result := LoadFromFile(aSrcFile) and
            SaveToFile(aDstFile, aMode, aConvertTo,
                       aEncProps, aResampler, aComments);
end;

function TSLConverter.PullStep : Integer;
begin
  Lock;
  try
    Result := InternalNextStep;
    if Result >= 0 then
    if Assigned(FOnProgress) then
    begin
      FOnProgress(Self, FConvdSize, FTotalSize, (Result <> 0));
    end;
  finally
    UnLock;
  end;
end;

procedure TSLConverter.PullBreak;
begin
  Lock;
  try
    FConverting := false;
  finally
    UnLock;
  end;
end;

{ TSLOutputFile }

procedure TSLOutputFile.InitEncoder(const aFileName : String;
                              aCodec : TSoundLiteCodecType);
begin
  SetFileInfo(TActiveSoundFileInfo.Create(aCodec, aFileName));
  TActiveSoundFileInfo(FFileInfo).SetFile(GenerateFileByCodec(aCodec));
end;

function TSLOutputFile.SoundFile : TSoundFile;
begin
  if not (FFileInfo is TActiveSoundFileInfo) then Exit(nil);
  Result := TActiveSoundFileInfo(FFileInfo).SoundFile;
end;

function TSLOutputFile.SaveToFile(const aFileName : String;
  aCodec : TSoundLiteCodecType; aProps : ISoundEncoderProps;
  aComments : ISoundComment) : Boolean;
begin
  InitEncoder(aFileName, aCodec);
  if Assigned(SoundFile) then
    Result := SoundFile.SaveToFile(aFileName, aProps, aComments) else
    Result := false;
end;

function TSLOutputFile.SaveToStream(Str : TStream;
  aCodec : TSoundLiteCodecType; aDataLimits : TSoundDataLimits;
  aProps : ISoundEncoderProps; aComments : ISoundComment) : Boolean;
begin
  InitEncoder(TSoundLite.FILENAME_GENERIC, aCodec);
  if Assigned(SoundFile) then
    Result := SoundFile.SaveToStream(Str, aDataLimits, aProps, aComments) else
    Result := false;
end;

function TSLOutputFile.SaveToStream(Str : TStream;
  aCodec : TSoundLiteCodecType; aProps : ISoundEncoderProps;
  aComments : ISoundComment) : Boolean;
begin
  InitEncoder(TSoundLite.FILENAME_GENERIC, aCodec);
  if Assigned(SoundFile) then
    Result := SoundFile.SaveToStream(Str, aProps, aComments) else
    Result := false;
end;

function TSLOutputFile.WriteData(Buffer : Pointer;
  aFrameSize : ISoundFrameSize; Ptr : Pointer) : ISoundFrameSize;
begin
  if Assigned(SoundFile) then
    Result := SoundFile.WriteData(Buffer, aFrameSize, Ptr) else
    Result := TOGLSound.NewErrorFrame;
end;

procedure TSLOutputFile.StopStreaming;
begin
  if Assigned(SoundFile) then
    SoundFile.StopStreaming;
end;

{ TSLFramedDecoder }

function TSLFramedDecoder.GetCurFrameID : QWord;
begin
  Result := FCurFrameID.Value;
end;

function TSLFramedDecoder.GetDataSource : TStream;
begin
  Lock;
  try
    Result := FDataSource;
  finally
    UnLock;
  end;
end;

function TSLFramedDecoder.GetTotalDuration : ISoundFrameSize;
begin
  Lock;
  try
    Result := FTotalDuration.Duplicate;
  finally
    UnLock;
  end;
end;

procedure TSLFramedDecoder.PushFrame;
begin
  if Assigned(FCurDecFrame) then
  begin
    FCurDecFrame.FrameSize := FCurFrameSize.Duplicate;
    FTotalDuration.Inc(FCurFrameSize);
    FCurFrameSize := FTotalDuration.EmptyDuplicate;
    FCurDecFrame.Position := 0;
    FDecodedFrames.Push_back(FCurDecFrame);
    FCurDecFrame := nil;
  end;
end;

procedure TSLFramedDecoder.RestartDecoder;
begin
  PushFrame;

  if Assigned(FDataSource) then
  begin
    FCurDecFrame := TSLAudioFrame.Create(CurrentFrameID);
    FCurFrameID.IncValue;

    if Assigned(FDecoder) then
    begin
      FDecoder.SetStream(FDataSource);
    end else
    begin
      if FDecCodec = TSoundLite.CODEC_OGG_VORBIS then
      begin
        FDecoder := TVorbis.NewOggStreamAltDecoder(FDataSource);
      end else
      if FDecCodec = TSoundLite.CODEC_OPUS then
      begin
        FDecoder := TOpus.NewStreamDecoder(FCurDecFrame, FDecProps);
      end else
      if FDecCodec = TSoundLite.CODEC_OGG_OPUS then
      begin
        FDecoder := TOpus.NewOggStreamDecoder(FDataSource,
                                                [sdpForceNotSeekable,
                                                 sdpReadOnly]);
      end else
      if FDecCodec = TSoundLite.CODEC_FLAC then
      begin
        FDecoder := TFLAC.NewStreamDecoder(FDataSource,
                                                [sdpForceNotSeekable,
                                                 sdpReadOnly]);
      end else
      if FDecCodec = TSoundLite.CODEC_OGG_FLAC then
      begin
        FDecoder := TFLAC.NewOggStreamDecoder(FDataSource,
                                                [sdpForceNotSeekable,
                                                 sdpReadOnly]);
      end else
        FDecoder := nil;
    end;
    if Assigned(FDecoder) then
    begin
      if not Assigned(FBuffer) then
      begin
        FBufferSize := FDecoder.EmptyFrame;
        TSoundLite.GetChunkSize(FBufferSize, FDecProps);
        FBuffer := GetMem(FBufferSize.AsBytes);
      end;
      FCurFrameSize := FDecoder.EmptyFrame;
      if FTotalDuration.IsErrored then
        FTotalDuration := FDecoder.EmptyFrame;
    end;
  end;
end;

procedure TSLFramedDecoder.SetDataSource(AValue : TStream);
begin
  Lock;
  try
    if AValue <> FDataSource then
    begin
      FDataSource := AValue;
      RestartDecoder;
    end;
  finally
    UnLock;
  end;
end;

constructor TSLFramedDecoder.Create(aDecCodec : TSoundLiteCodecType;
  aDecProps : ISoundProps);
begin
  inherited Create;
  FCurFrameID := TThreadQWord.Create(0);
  FTotalDuration := TOGLSound.NewErrorFrame;
  FDecodedFrames := TSLAudioFrames.Create;
  FDecProps := aDecProps;
  FDataSource := nil;
  FCurDecFrame := nil;
  FDecoder := nil;
  FDecCodec := aDecCodec;
end;

destructor TSLFramedDecoder.Destroy;
begin
  if Assigned(FCurDecFrame) then
    FreeAndNil(FCurDecFrame);
  FDecodedFrames.Free;
  FCurFrameID.Free;
  FDecoder := nil;
  FDecProps := nil;
  if Assigned(FBuffer) then
    FreeMemAndNil(FBuffer);
  inherited Destroy;
end;

function TSLFramedDecoder.VorbisOggDecoder : TVorbisOggStreamAltDecoder;
begin
  if FDecCodec = TSoundLite.CODEC_OGG_VORBIS then
    Result := FDecoder as TVorbisOggStreamAltDecoder else
    Result := nil;
end;

function TSLFramedDecoder.OpusOggDecoder : TOpusOggStreamDecoder;
begin
  if FDecCodec = TSoundLite.CODEC_OGG_OPUS then
    Result := FDecoder as TOpusOggStreamDecoder else
    Result := nil;
end;

function TSLFramedDecoder.OpusDecoder : TOpusStreamDecoder;
begin
  if FDecCodec = TSoundLite.CODEC_OPUS then
    Result := FDecoder as TOpusStreamDecoder else
    Result := nil;
end;

function TSLFramedDecoder.FLACOggDecoder : TFLACOggStreamDecoder;
begin
  if FDecCodec = TSoundLite.CODEC_OGG_FLAC then
    Result := FDecoder as TFLACOggStreamDecoder else
    Result := nil;
end;

function TSLFramedDecoder.FLACDecoder : TFLACStreamDecoder;
begin
  if FDecCodec = TSoundLite.CODEC_FLAC then
    Result := FDecoder as TFLACStreamDecoder else
    Result := nil;
end;

function TSLFramedDecoder.DecodeData(Count : ISoundFrameSize;
                                           Par : Pointer) : ISoundFrameSize;
var
  cur, readed, size : ISoundFrameSize;
begin
  Lock;
  try
    Result := Count.EmptyDuplicate;
    size := Count.Duplicate;
    repeat
      if size.Greater(FBufferSize) then
        cur := FBufferSize else
        cur := size;
      readed := FDecoder.ReadData(FBuffer, cur, Par);
      if readed.IsEmptyOrErrored then
        break;
      FCurDecFrame.Write(FBuffer^, readed.AsBytes);
      Result.Inc(readed);
      FCurFrameSize.Inc(readed);
      size.Dec(readed);
    until size.IsEmpty;
  finally
    UnLock;
  end;
end;

function TSLFramedDecoder.DecodeAllData(Par : Pointer) : ISoundFrameSize;
var
  readed : ISoundFrameSize;
begin
  Lock;
  try
    Result := FDecoder.EmptyFrame;
    while true do
    begin
      readed := FDecoder.ReadData(FBuffer, FBufferSize, Par);
      if readed.IsEmptyOrErrored then
        break;
      FCurDecFrame.Write(FBuffer^, readed.AsBytes);
      Result.Inc(readed);
      FCurFrameSize.Inc(readed);
    end;
  finally
    UnLock;
  end;
end;

procedure TSLFramedDecoder.Flush;
begin
  DataSource := nil;
  if (FDecCodec and $FFFFF0) = TSoundLite.CODEC_FLAC then
  begin
    (FDecoder as TFLACAbstractDecoder).Ref.Flush;
  end;
end;

function TSLFramedDecoder.Ready : Boolean;
begin
  Result := Assigned(FDataSource) and Assigned(FDecoder);
end;

{ TSLFramedEncoder }

function TSLFramedEncoder.GetCurFrameID : QWord;
begin
  Result := FCurFrameID.Value;
end;

function TSLFramedEncoder.GetMaxFrameSize : ISoundFrameSize;
begin
  Lock;
  try
    Result := FMaxFrameSize.Duplicate;
  finally
    UnLock;
  end;
end;

function TSLFramedEncoder.GetTotalDuration : ISoundFrameSize;
begin
  Lock;
  try
    Result := FTotalDuration.Duplicate;
  finally
    UnLock;
  end;
end;

procedure TSLFramedEncoder.PushNext;
begin
  Lock;
  try
    if Assigned(Fencoder) then
      FEncoder.Flush(nil);
    PushFrame;

    FCurEncFrame := TSLEncodedFrame.Create(GetCurFrameID, FEncCodec);
    RestartEncoder;
  finally
    UnLock;
  end;
end;

procedure TSLFramedEncoder.PushFrame;
begin
  Lock;
  try
    if Assigned(FCurEncFrame) then
    begin
      FCurEncFrame.FrameSize := FCurFrameSize;
      FTotalDuration.Inc(FCurFrameSize);
      FEncodedFrames.Push_back(FCurEncFrame);
      FCurFrameID.IncValue;
    end;
    FCurEncFrame := nil;
  finally
    UnLock;
  end;
end;

procedure TSLFramedEncoder.Init(aEncCodec : TSoundLiteCodecType;
  aEncProps : ISoundEncoderProps; aEncComm : ISoundComment);
begin
  FEncodedFrames := TSLEncodedFrames.Create;
  FCurEncFrame := nil;
  FCurFrameID := TThreadQWord.Create(0);

  FEncCodec := aEncCodec;
  FEncComments := aEncComm;
  FEncProps := aEncProps;
  FCurEncFrame := nil;
  FEncoder := nil;
  PushNext;

  if Assigned(FEncoder) then
  begin
    // need to write header
    if FEncCodec in [TSoundLite.CODEC_OGG_OPUS,
                     TSoundLite.CODEC_OGG_FLAC,
                     TSoundLite.CODEC_FLAC,
                     TSoundLite.CODEC_WAV, TSoundLite.CODEC_OGG_WAV,
                     TSoundLite.CODEC_OGG_VORBIS] then
      FEncoder.WriteHeader(nil);

    FTotalDuration := FEncoder.EmptyFrame;
    FMaxFrameSize := FTotalDuration.Duplicate;
    TSoundLite.GetMaxFrameSize(FMaxFrameSize, FEncProps);
  end else
  begin
    FTotalDuration := TOGLSound.NewErrorFrame;
    FMaxFrameSize  := TOGLSound.NewErrorFrame;
    FCurFrameSize  := TOGLSound.NewErrorFrame;
  end;
end;

procedure TSLFramedEncoder.RestartEncoder;
begin
  if Assigned(FEncoder) then
  begin
    FEncoder.SetStream(FCurEncFrame);
  end else
  begin
    if FEncCodec = TSoundLite.CODEC_OGG_VORBIS then
    begin
      FEncoder := TVorbis.NewOggStreamEncoder(FCurEncFrame,
                                              [sdpForceNotSeekable, sdpWriteOnly],
                                              FEncProps, FEncComments);
    end else
    if FEncCodec = TSoundLite.CODEC_OPUS then
    begin
      FEncoder := TOpus.NewStreamEncoder(FCurEncFrame, FEncProps);
    end else
    if FEncCodec = TSoundLite.CODEC_OGG_OPUS then
    begin
      FEncoder := TOpus.NewAltOggStreamEncoder(FCurEncFrame, FEncProps, FEncComments);
    end else
    if FEncCodec = TSoundLite.CODEC_WAV then
    begin
      FEncoder := TWAVE.NewStreamEncoder(FCurEncFrame,
                                              [sdpForceNotSeekable, sdpWriteOnly],
                                              FEncProps, nil);
    end else
    if FEncCodec = TSoundLite.CODEC_OGG_WAV then
    begin
      FEncoder := TWAVE.NewOggStreamEncoder(FCurEncFrame,
                                              [sdpForceNotSeekable, sdpWriteOnly],
                                              FEncProps, nil);
    end else
    if FEncCodec = TSoundLite.CODEC_FLAC then
    begin
      FEncoder := TFLAC.NewStreamEncoder(FCurEncFrame,
                                              [sdpForceNotSeekable, sdpWriteOnly],
                                              FEncProps, FEncComments);
    end else
    if FEncCodec = TSoundLite.CODEC_OGG_FLAC then
    begin
      FEncoder := TFLAC.NewOggStreamEncoder(FCurEncFrame,
                                              [sdpForceNotSeekable, sdpWriteOnly],
                                              FEncProps, FEncComments);
    end else
      FEncoder := nil;
  end;
  if Assigned(FEncoder) then
    FCurFrameSize := FEncoder.EmptyFrame;
end;

procedure TSLFramedEncoder.SetMaxFrameSize(AValue : ISoundFrameSize);
begin
  Lock;
  try
    FMaxFrameSize := AValue.Duplicate;
  finally
    UnLock;
  end;
end;

constructor TSLFramedEncoder.Create(aEncCodec : TSoundLiteCodecType;
  aEncProps : ISoundEncoderProps);
begin
  inherited Create;

  Init(aEncCodec, aEncProps, nil);
end;

constructor TSLFramedEncoder.Create(aEncCodec : TSoundLiteCodecType;
  aEncProps : ISoundEncoderProps; aEncComm : ISoundComment);
begin
  inherited Create;

  Init(aEncCodec, aEncProps, aEncComm);
end;

destructor TSLFramedEncoder.Destroy;
begin
  if Assigned(FCurEncFrame) then
    FreeAndNil(FCurEncFrame);
  FEncodedFrames.Free;
  FCurFrameID.Free;
  inherited Destroy;
end;

function TSLFramedEncoder.VorbisOggEncoder : TVorbisOggStreamEncoder;
begin
  if FEncCodec = TSoundLite.CODEC_OGG_VORBIS then
    Result := FEncoder as TVorbisOggStreamEncoder else
    Result := nil;
end;

function TSLFramedEncoder.OpusOggEncoder : TOpusOggStreamEncoder;
begin
  if FEncCodec = TSoundLite.CODEC_OGG_OPUS then
    Result := FEncoder as TOpusOggStreamEncoder else
    Result := nil;
end;

function TSLFramedEncoder.OpusEncoder : TOpusStreamEncoder;
begin
  if FEncCodec = TSoundLite.CODEC_OPUS then
    Result := FEncoder as TOpusStreamEncoder else
    Result := nil;
end;

function TSLFramedEncoder.FLACOggEncoder : TFLACOggStreamEncoder;
begin
  if FEncCodec = TSoundLite.CODEC_OGG_FLAC then
    Result := FEncoder as TFLACOggStreamEncoder else
    Result := nil;
end;

function TSLFramedEncoder.FLACEncoder : TFLACStreamEncoder;
begin
  if FEncCodec = TSoundLite.CODEC_FLAC then
    Result := FEncoder as TFLACStreamEncoder else
    Result := nil;
end;

function TSLFramedEncoder.WriteData(Buffer : Pointer; Count : ISoundFrameSize;
  Par : Pointer) : ISoundFrameSize;
begin
  Lock;
  try
    if Ready then
    begin
      Result := FEncoder.WriteData(Buffer, Count, Par);
      if Result.IsValid then
      begin
        FCurFrameSize.Inc(Result);
        if FCurFrameSize.GreaterOrEqual(FMaxFrameSize) then
        begin
          PushNext;
        end;
      end;
    end else
      Result := TOGLSound.NewErrorFrame;
  finally
    UnLock;
  end;
end;

procedure TSLFramedEncoder.Close(Par : Pointer);
begin
  Lock;
  try
    FEncoder.Close(Par);
    PushFrame;
  finally
    UnLock;
  end;
end;

procedure TSLFramedEncoder.Flush(Par : Pointer);
begin
  Lock;
  try
    FEncoder.Flush(Par);
    PushFrame;
  finally
    UnLock;
  end;
end;

function TSLFramedEncoder.Ready : Boolean;
begin
  Lock;
  try
    if not Assigned(FEncoder) then Exit(false);
    Result := FEncoder.Ready;
  finally
    UnLock;
  end;
end;

{ TSLAudioFrame }

procedure TSLAudioFrame.SetFrameSize(AValue : ISoundFrameSize);
begin
  FFrame := AValue.Duplicate;
end;

constructor TSLAudioFrame.Create(aID : QWord);
begin
  inherited Create;
  FId := aID;
  FFrame := TOGLSound.NewErrorFrame;
end;

{ TSLEncodedFrame }

constructor TSLEncodedFrame.Create(aID : QWord; aCodec : TSoundLiteCodecType);
begin
  inherited Create(aID);
  FCodec := aCodec;
end;

{ TSLPlaylistConsumers }

destructor TSLPlaylistConsumers.Destroy;
begin
  ExtractAll;
  inherited Destroy;
end;

{ TSLPlayerThread }

constructor TSLPlayerThread.Create;
begin
  inherited Create(true);
  FPlayer := TSLPlayer.Create;
  FPlayer.InitPlayer;
  FreeOnTerminate := false;
end;

constructor TSLPlayerThread.Create(const devicename : String);
begin
  inherited Create(true);
  FPlayer := TSLPlayer.Create;
  FPlayer.InitPlayer(devicename);
  FreeOnTerminate := false;
end;

constructor TSLPlayerThread.Create(const devicename : String; buffers,
  buffersize : Integer);
begin
  inherited Create(true);
  FPlayer := TSLPlayer.Create;
  FPlayer.InitPlayer(devicename, buffers, buffersize);
  FreeOnTerminate := false;
end;

destructor TSLPlayerThread.Destroy;
begin
  FPlayer.Free;
  inherited Destroy;
end;

procedure TSLPlayerThread.Execute;
begin
  while not Terminated do
  begin
    FPlayer.Proceed;
    Sleep(100);
  end;
  FPlayer.Stop;
end;

{ TSLTrackDataSource }

constructor TSLTrackDataSource.Create;
begin
  inherited Create;
  FStream := TSLTrackFile.Create;
  FOwnFile := true;
end;

constructor TSLTrackDataSource.Create(aTrackFile : TSLTrackFile;
  aOwnFile : Boolean);
begin
  inherited Create;
  FStream := aTrackFile;
  FOwnFile := aOwnFile;
end;

destructor TSLTrackDataSource.Destroy;
begin
  if FOwnFile and Assigned(FStream) then
    FreeAndNil(FStream);
  inherited Destroy;
end;

function TSLTrackDataSource.LoadFromFile(const Fn : String) : Boolean;
begin
  Result := FStream.LoadFromFile(Fn, true);
end;

function TSLTrackDataSource.LoadFromStream(Str : TStream) : Boolean;
begin
  Result := FStream.LoadFromStream(Str, true);
end;

procedure TSLTrackDataSource.SeekSample(aSample : Integer);
begin
  if FStream.Active then
    FStream.SeekSample(aSample);
end;

procedure TSLTrackDataSource.SeekTime(aTime : Double);
begin
  if FStream.Active then
    FStream.SeekTime(aTime);
end;

function TSLTrackDataSource.TellSamples : Integer;
begin
  if FStream.Active then
    Result := FStream.GetDecodedSamples else
    Result := 0;
end;

function TSLTrackDataSource.TotalSamples : Integer;
begin
  if FStream.Active then
    Result := FStream.GetTotalSamples else
    Result := 0;
end;

function TSLTrackDataSource.ReadChunk(const Buffer : Pointer; Pos : Int64;
  Sz : Integer; isloop : Boolean; var fmt : TOALFormat; var freq : Cardinal
  ) : Integer;
begin
  fmt := TOpenAL.OALFormat(FStream.FileInfo.Channels, FStream.FileInfo.Bitdepth);
  freq := FStream.FileInfo.Frequency;

  if FStream.Active then
  begin

    Result := FStream.ReadData(Buffer, FStream.FileInfo.Codec.FrameFromBytes(Sz),
                                       nil).AsBytes;
    if (Result = 0) and isloop then
    begin
      FStream.ResetToStart;
      Result := FStream.ReadData(Buffer, FStream.FileInfo.Codec.FrameFromBytes(Sz),
                                         nil).AsBytes;
    end;
  end else
    Result := 0;
end;

{ TSLPlayListTrackDataSource }

procedure TSLPlayListTrackDataSource.AfterApplied;
begin
  if Assigned(FStream) then
  begin
    FStream.Activate;
    FStream.ResetToStart;
  end;
end;

constructor TSLPlayListTrackDataSource.Create(aPos : Integer;
  aPlayList : TSLPlayList);
var
  aTrackFile : TSLTrackFile;
begin
  aTrackFile := aPlayList[aPos];
  inherited Create(aTrackFile, false);
end;

destructor TSLPlayListTrackDataSource.Destroy;
begin
  if Assigned(FStream) then
    FStream.StandBy;
  inherited Destroy;
end;

function TSLPlayListTrackDataSource.LoadFromFile(const {%H-}Fn : String) : Boolean;
begin
  Result := false;
end;

function TSLPlayListTrackDataSource.LoadFromStream({%H-}Str : TStream) : Boolean;
begin
  Result := false;
end;

{ TSLPlayer }

procedure TSLPlayer.DoStatusChanged(aNew, aOld : TSLPlayerState);
begin
  FTrackFinished := (FOALPlayer.Stream.PlayedSamples >= (FOALPlayer.Stream.DecodedSamples-1)) and
                    (FOALPlayer.Stream.PlayedSamples >= (FOALPlayer.Stream.TotalSamples-1));
  inherited DoStatusChanged(aNew, aOld);
end;

procedure TSLPlayer.DoClearPlayList;
begin
  Lock;
  try
    Stop;
    FOALPlayer.Stream.DataSource := nil;
  finally
    Unlock;
  end;
end;

procedure TSLPlayer.DoDeleteTrack(aTrack : TSLTrackFile);
begin
  Lock;
  try
    if TSLPlayListTrackDataSource(FOALPlayer.Stream.DataSource).FStream = aTrack then
    begin
      Stop;
      FOALPlayer.Stream.DataSource := nil;
    end;
  finally
    Unlock;
  end;
end;

constructor TSLPlayer.Create;
begin
  inherited Create;
  FConsumer := TSLPlaylistConsumer.Create;
  FConsumer.OnClearPlaylist := @DoClearPlayList;
  FConsumer.OnDeleteTrack := @DoDeleteTrack;
  FPlayList := TSLPlayList.Create;
  FPlayList.AddConsumer(FConsumer);
end;

destructor TSLPlayer.Destroy;
begin
  FPlayList.Free;
  FConsumer.Free;
  inherited Destroy;
end;

class function TSLPlayer.StartThread : TSLPlayerThread;
begin
  Result := TSLPlayerThread.Create;
  Result.Start;
end;

procedure TSLPlayer.FullLock;
begin
  Lock;
  FPlayList.Lock;
end;

procedure TSLPlayer.FullUnLock;
begin
  FPlayList.UnLock;
  UnLock;
end;

procedure TSLPlayer.Play;
var
  ADataSrc : TSLPlayListTrackDataSource;
  AActiveTrack, ASelectTrack : TSLTrackFile;
  Restart : Boolean;
begin
  FullLock;
  try
    if FPlayList.PlayPosition >= 0 then
    begin
      if FOALPlayer.Stream.DataSource is TSLPlayListTrackDataSource then
      begin
        AActiveTrack := TSLPlayListTrackDataSource(FOALPlayer.Stream.DataSource).FStream;
      end else
        AActiveTrack := nil;
      ASelectTrack := FPlayList.CurrentTrack;

      if ASelectTrack = AActiveTrack then
      begin
        if Status = slsPaused then
        begin
          Resume;
          Restart := false;
        end else
          Restart := true;
      end else
        Restart := true;

      if Restart then
      begin
        FOALPlayer.Stop;
        FTrackFinished := false;
        ADataSrc := TSLPlayListTrackDataSource.Create(FPlayList.PlayPosition,
                                                      FPlayList);
        FOALPlayer.Stream.DataSource := ADataSrc;
        FOALPlayer.Play;
        FLastStatus := slsPlaying;
        if Assigned(FOnStartPlay) then
          FOnStartPlay(Self);
      end;
    end;
  finally
    FullUnLock;
  end;
end;

procedure TSLPlayer.Proceed;
var
  FStatus : TSLPlayerState;
begin
  Lock;
  try
    FStatus := Status;
    if FStatus = slsPlaying then
      FOALPlayer.Stream.Proceed
    else
    if FTrackFinished then
    begin
      FPlayList.Next;
      Play;
    end;
  finally
    UnLock;
  end;
end;

{ TSLPlayList }

procedure TSLPlayList.DoClear(obj : TObject);
begin
  TSLPlaylistConsumer(obj).DoClearPlayList;
end;

procedure TSLPlayList.DoAddTrack(obj : TObject; data : Pointer);
begin
  TSLPlaylistConsumer(obj).DoAddTrack(TSLTrackFile(data));
end;

procedure TSLPlayList.DoAddTracks(obj : TObject; data : Pointer);
begin
  TSLPlaylistConsumer(obj).DoAddTracks(PInteger(data)^);
end;

procedure TSLPlayList.DoDeleteTrack(obj : TObject; data : Pointer);
begin
  TSLPlaylistConsumer(obj).DoDeleteTrack(TSLTrackFile(data));
end;

function TSLPlayList.GetListPlayPosition : Integer;
begin
  Result := GetListTrackPos(FOrderPlayPosition);
end;

procedure TSLPlayList.SendToAllConsumers(act : TSLPlaylistAction;
  atrack : TSLTrackFile);
var c : integer;
begin
  case act of
    slplaClear :    FConsumers.DoForAll(@DoClear);
    slplaAddTrack : FConsumers.DoForAllEx(@DoAddTrack, atrack);
    slplaAddTracks : begin
      c := Count;
      FConsumers.DoForAllEx(@DoAddTracks, @c);
    end;
    slplaDeleteTrack : FConsumers.DoForAllEx(@DoDeleteTrack, atrack);
  end;
end;

function TSLPlayList.GetListTrackPos(OrderPos : Integer) : Integer;
begin
  if OrderPos < 0 then Exit(-1);
  if OrderPos >= FPlayOrder.Count then Exit(-1);
  Result := FPlayOrder[OrderPos];
end;

function TSLPlayList.GetOrderTrackPos(ListPos : Integer) : Integer;
var i : integer;
begin
  for i := 0 to FPlayOrder.Count-1 do
  begin
    if FPlayOrder[i] = ListPos then
      Exit(i);
  end;
  Result := -1;
end;

procedure TSLPlayList.DeleteLastFromOrder;
var ind : Integer;
begin
  ind := FPlayOrder.IndexOf(FPlayOrder.Count-1);
  if ind >= 0 then
    FPlayOrder.Delete(ind);
end;

procedure TSLPlayList.AddSilent(const Obj : TSLTrackFile);
begin
  FPlayOrder.Add(Count);
  inherited Add(Obj);
  if FPlayShuffle then
    DoShuffle;
end;

procedure TSLPlayList.SetListPlayPosition(AValue : Integer);
begin
  Lock;
  try
    SetOrderPlayPosition(GetOrderTrackPos(AValue));
  finally
    Unlock;
  end;
end;

procedure TSLPlayList.SetOrderPlayPosition(AValue : Integer);
var lstTrack, curTrack : TSLTrackFile;
begin
  Lock;
  try
    if FOrderPlayPosition = AValue then Exit;

    lstTrack := CurrentTrack;

    FOrderPlayPosition := AValue;

    if Assigned(FOnTrackChanged) then
    begin
      curTrack := CurrentTrack;
      if curTrack <> lstTrack then
        FOnTrackChanged(Self, lstTrack, curTrack);
    end;
  finally
    Unlock;
  end;
end;

procedure TSLPlayList.DoShuffle;
var
  i, i2, p : integer;
begin
  if FPlayOrder.Count > 2 then
  for i := 0 to FPlayOrder.Count-1 do
  if i <> FOrderPlayPosition then
  begin
    repeat
      i2 := Random(FPlayOrder.Count);
      if (i2 <> FOrderPlayPosition) and (i <> i2) then
      begin
        p :=  FPlayOrder[i];
        FPlayOrder[i] := FPlayOrder[i2];
        FPlayOrder[i2] := p;
        break;
      end;
    until false;
  end;
end;

procedure TSLPlayList.SetPlayShuffle(AValue : Boolean);
var
  i, p : integer;
begin
  Randomize;
  Lock;
  try
    if FPlayShuffle = AValue then Exit;

    FPlayShuffle := AValue;

    if AValue then
    begin
      DoShuffle;
    end else
    begin
      p := GetListTrackPos(FOrderPlayPosition);
      FPlayOrder.Clear;
      for i := 0 to Count-1 do
        FPlayOrder.Add(i);
      PlayPosition := p;
    end;
  finally
    Unlock;
  end;
end;

constructor TSLPlayList.Create;
begin
  inherited Create;
  FOrderPlayPosition := -1;
  FPlayRepeat := false;
  FPlayShuffle := false;
  FPlayOrder := TFastIntegerList.Create;
  FConsumers := TSLPlaylistConsumers.Create;
end;

destructor TSLPlayList.Destroy;
begin
  Clear;
  FPlayOrder.Free;
  FConsumers.Free;
  inherited Destroy;
end;

function TSLPlayList.LoadFromJSON(aJSON : TJSONObject) : Boolean;
var
  d : TJSONData;
  i : integer;
  track : TSLTrackFile;
begin
  Clear;
  if not Assigned(aJSON) then Exit(false);
  Lock;
  try
    if aJSON.Find(JSON_REPEAT, d) then
      FPlayRepeat := d.AsBoolean else
      FPlayRepeat := false;

    if aJSON.Find(JSON_TRACKS, d) then
    begin
      for i := 0 to TJSONArray(d).Count-1 do
      begin
        track := TSLTrackFile.Create;
        track.LoadFromJSON(TJSONObject(TJSONArray(d)[i]));
        AddSilent(track);
      end;
      if Count > 0 then
        SendToAllConsumers(slplaAddTracks, nil);
    end;

    if aJSON.Find(JSON_SHUFFLE, d) then
    begin
      if d.AsBoolean then
        Shuffle else
        FPlayShuffle := false;
    end else
      FPlayShuffle := false;

    if aJSON.Find(JSON_POS, d) then
      PlayPosition := d.AsInteger else
    begin
      if Count > 0 then
        PlayPosition := 0 else
        PlayPosition := -1;
    end;

  finally
    UnLock;
  end;
end;

function TSLPlayList.LoadFromFile(const Fn : String) : Boolean;
var
  FS : TFileStream;
begin
  FS := TFileStream.Create(Fn, fmOpenRead);
  try
    Result := LoadFromStream(FS);
  finally
    FS.Free;
  end;
end;

function TSLPlayList.LoadFromStream(Str : TStream) : Boolean;
const CHUNK_SIZE : Cardinal = 512;
var
  js : TJSONData;
  Buffer : Pointer;
  S : RawByteString;
  L, SC : Integer;
begin
  Result := false;
  Buffer := GetMem(CHUNK_SIZE);
  try
    L := 0;
    repeat
      SC := Str.Read(Buffer^, CHUNK_SIZE);
      if SC > 0 then
      begin
        SetLength(S, L + SC);
        Move(PByte(Buffer)^, S[L+1], SC);
        Inc(L, SC);
      end;
    until SC < CHUNK_SIZE;
  finally
    Freemem(Buffer);
  end;

  try
    js := GetJSON(UTF8Encode(S));
    if Assigned(js) then
    try
      if js is TJSONObject then
        Result := LoadFromJSON(TJSONObject(js))
      else
        Result := False;
    finally
      js.Free;
    end else
      Result := False;
  except
    on e : EJSONParser do Result := False;
  end;
end;

function TSLPlayList.SaveToJSON : TJSONObject;
var
  i : integer;
  tracks : TJSONArray;
  track : TJSONObject;
begin
  Lock;
  try
    tracks := TJSONArray.Create;
    for i := 0 to Count-1 do
    begin
      track := Self[i].FileInfo.SaveToJSON;
      if assigned(track) then
        tracks.Add(track);
    end;

    Result := TJSONObject.Create([JSON_POS, PlayPosition,
                                  JSON_REPEAT, FPlayRepeat,
                                  JSON_SHUFFLE, FPlayShuffle,
                                  JSON_TRACKS, tracks]);

  finally
    UnLock;
  end;
end;

function TSLPlayList.SaveToFile(const Fn : String) : Boolean;
var
  FS : TFileStream;
begin
  FS := TFileStream.Create(Fn, fmOpenWrite or fmCreate);
  try
    Result := SaveToStream(FS);
  finally
    FS.Free;
  end;
end;

function TSLPlayList.SaveToStream(Str : TStream) : Boolean;
var
  js : TJSONObject;
  S : String;
  L : Integer;
begin
  js := SaveToJSON;
  if assigned(js) then
  begin
    try
      js.CompressedJSON := false;
      S := js.AsJSON;
      L := Length(S);
      if L > 0  then
      begin
        Str.WriteBuffer(S[1], L);
        Result := true;
      end else
        Result := false;
    finally
      js.Free;
    end;
  end else
    Result := false;
end;

procedure TSLPlayList.AddConsumer(cons : TSLPlaylistConsumer);
begin
  FConsumers.Push_back(cons);
end;

function TSLPlayList.Add(const Obj : TSLTrackFile) : Integer;
begin
  if Assigned(obj) then
    SendToAllConsumers(slplaAddTrack, obj);
  FPlayOrder.Add(Count);
  Result := inherited Add(Obj);
  if FPlayShuffle then
    DoShuffle;
end;

function TSLPlayList.AddFromFile(const FN : String) : Integer;
var
  aFile : TSLTrackFile;
begin
  aFile := TSLTrackFile.Create;
  if aFile.LoadFromFile(FN, false) then
  begin
    Result := Add(aFile);
  end else
  begin
    FreeAndNil(aFile);
    Result := -1;
  end;
end;

procedure TSLPlayList.Delete(Ind : integer);
var p : integer;
begin
  if (Ind >= 0) and (ind <= Count) then
    SendToAllConsumers(slplaDeleteTrack, Item[Ind]);
  Lock;
  try
    if Ind = PlayPosition then
       PlayPosition := -1;
    p := GetListTrackPos(FOrderPlayPosition);
    if p >= Ind then Dec(p);
    inherited Delete(Ind);
    DeleteLastFromOrder;
    FOrderPlayPosition := GetOrderTrackPos(p);
  finally
    UnLock;
  end;
end;

procedure TSLPlayList.Clear;
begin
  if Count > 0 then
    SendToAllConsumers(slplaClear, nil);
  Lock;
  try
    PlayPosition := -1;
    inherited Clear;
    FPlayOrder.Clear;
  finally
    UnLock;
  end;
end;

procedure TSLPlayList.Next;
begin
  Lock;
  try
    if FOrderPlayPosition >= 0 then
    begin
      if FOrderPlayPosition < (FPlayOrder.Count-1) then
        SetOrderPlayPosition(FOrderPlayPosition + 1) else
      if FPlayRepeat then
      begin
        if FPlayOrder.Count > 0 then
          SetOrderPlayPosition(0) else
          PlayPosition := -1;
      end else
        PlayPosition := -1;
    end;
  finally
    UnLock;
  end;
end;

procedure TSLPlayList.Prev;
begin
  Lock;
  try
    if FOrderPlayPosition > 0 then
      SetOrderPlayPosition(FOrderPlayPosition - 1) else
    if FPlayRepeat then
    begin
      if FPlayOrder.Count > 0 then
        SetOrderPlayPosition(FPlayOrder.Count-1) else
        PlayPosition := -1;
    end else
      PlayPosition := -1;
  finally
    UnLock;
  end;
end;

function TSLPlayList.CurrentTrack : TSLTrackFile;
begin
  Lock;
  try
    if (FOrderPlayPosition >= 0) and (FOrderPlayPosition < FPlayOrder.Count) then
    begin
      Result := Self[PlayPosition];
    end else
      Result := nil;
  finally
    UnLock;
  end;
end;

procedure TSLPlayList.Shuffle;
begin
  SetPlayShuffle(True);
end;


{ ESLCantActivateFile }

class function ESLCantActivateFile.ErrorNo : Cardinal;
begin
  Result := ecCAN_NOT_ACTIVATE;
end;

{ ESLFileNotActivated }

class function ESLFileNotActivated.ErrorNo : Cardinal;
begin
  Result := ecFILE_NOT_ACTIVATED;
end;

{ ESLErroredFileInfo }

class function ESLErroredFileInfo.ErrorNo : Cardinal;
begin
  Result := ecERRORED_FILE_INFO;
end;

{ ESoundLiteException }

constructor ESoundLiteException.Create;
begin
  inherited Create(esLISTED[ErrorNo]);
end;

class function ESoundLiteException.ErrorNo : Cardinal;
begin
  Result := ecUNKNOWN_ERROR;
end;

{ TErroredSoundFileInfo }

function TErroredSoundFileInfo.Comments : ISoundComment;
begin
  Result := nil;
  raise ESLErroredFileInfo.Create;
end;

function TErroredSoundFileInfo.Stream : TStream;
begin
  Result := nil;
  raise ESLErroredFileInfo.Create;
end;

function TErroredSoundFileInfo.Codec : ISoundEncDec;
begin
  Result := nil;
  raise ESLErroredFileInfo.Create;
end;

function TErroredSoundFileInfo.SaveToJSON : TJSONObject;
begin
  Result := nil;
end;

function TErroredSoundFileInfo.Frequency : Cardinal;
begin
  Result := 0;
  raise ESLErroredFileInfo.Create;
end;

function TErroredSoundFileInfo.Bitrate : Cardinal;
begin
  Result := 0;
  raise ESLErroredFileInfo.Create;
end;

function TErroredSoundFileInfo.Bitdepth : Cardinal;
begin
  Result := 0;
  raise ESLErroredFileInfo.Create;
end;

function TErroredSoundFileInfo.SampleSize : TSoundSampleSize;
begin
  Result := ss16bit;
  raise ESLErroredFileInfo.Create;
end;

function TErroredSoundFileInfo.Channels : Cardinal;
begin
  Result := 0;
  raise ESLErroredFileInfo.Create;
end;

function TErroredSoundFileInfo.Version : Cardinal;
begin
  Result := 0;
  raise ESLErroredFileInfo.Create;
end;

function TErroredSoundFileInfo.TimeTotal : Double;
begin
  Result := 0;
  raise ESLErroredFileInfo.Create;
end;

function TErroredSoundFileInfo.SamplesTotal : Integer;
begin
  Result := 0;
  raise ESLErroredFileInfo.Create;
end;

{ TActiveSoundFileInfo }

procedure TActiveSoundFileInfo.SetFile(aFile : TSoundFile);
begin
  if Assigned(FFile) then
    FreeAndNil(FFile);
  FFile := aFile;
end;

destructor TActiveSoundFileInfo.Destroy;
begin
  SetFile(nil);
  inherited Destroy;
end;

function TActiveSoundFileInfo.Comments : ISoundComment;
begin
  if Assigned(FFile) and Assigned(Codec) then
  begin
    Result := Codec.Comments;
  end
  else
    Result := nil;
end;

function TActiveSoundFileInfo.Stream : TStream;
begin
  if Assigned(FFile) then
    Result := FFile.Stream
  else
    Result := nil;
end;

function TActiveSoundFileInfo.Codec : ISoundEncDec;
begin
  if Assigned(FFile) then
  begin
    if FFile.EncoderReady then
      Result := FFile.Encoder else
    if FFile.DecoderReady then
      Result := FFile.Decoder else
      Result := nil;
  end
  else
    Result := nil;
end;

function TActiveSoundFileInfo.Frequency : Cardinal;
begin
  if Assigned(FFile) then
    Result := FFile.Frequency
  else
    Result := 0;
end;

function TActiveSoundFileInfo.Bitrate : Cardinal;
begin
  if Assigned(FFile) then
    Result := FFile.Bitrate
  else
    Result := 0;
end;

function TActiveSoundFileInfo.Bitdepth : Cardinal;
begin
  if Assigned(FFile) then
    Result := FFile.Bitdepth
  else
    Result := 0;
end;

function TActiveSoundFileInfo.SampleSize : TSoundSampleSize;
begin
  if Assigned(FFile) then
    Result := FFile.SampleSize
  else
    Result := ss16bit;
end;

function TActiveSoundFileInfo.Channels : Cardinal;
begin
  if Assigned(FFile) then
    Result := FFile.Channels
  else
    Result := 0;
end;

function TActiveSoundFileInfo.Version : Cardinal;
begin
  if Assigned(FFile) then
    Result := FFile.Version
  else
    Result := 0;
end;

function TActiveSoundFileInfo.TimeTotal : Double;
begin
  if Assigned(FFile) then
    Result := FFile.Decoder.TimeTotal
  else
    Result := 0.0;
end;

function TActiveSoundFileInfo.SamplesTotal : Integer;
begin
  if Assigned(FFile) then
    Result := FFile.Decoder.SampleTotal
  else
    Result := 0;
end;

{ TPassiveSoundStreamInfo }

destructor TPassiveSoundStreamInfo.Destroy;
begin
  if Assigned(FStream) then
    FreeAndNil(FStream);
  inherited Destroy;
end;

procedure TPassiveSoundStreamInfo.SetStream(aStr : TStream);
begin
  if Assigned(FStream) then
    FreeAndNil(FStream);
  FStream := aStr;
end;

function TPassiveSoundStreamInfo.DetachStream : TStream;
begin
  Result := FStream;
  FStream := nil;
end;

{ TPassiveSoundFileInfo }

procedure TPassiveSoundFileInfo.SetComments(aComments : ISoundComment);
begin
  FComments := TOGLSound.NewVorbisComment(aComments);
end;

procedure TPassiveSoundFileInfo.SetProps(aFreq, aChannels, aBitrate,
  aVersion : Cardinal; aTotalSize : Double; aSampleSize : TSoundSampleSize);
begin
  FFreq := aFreq;
  FChannels := aChannels;
  FBitrate := aBitrate;
  FVersion := aVersion;
  FTotalSize := aTotalSize;
  FSampleSize := aSampleSize;
end;

constructor TPassiveSoundFileInfo.Create(aSrc : TJSONObject);
var
  aCodecType : TSoundLiteCodecType;
  aFileName  : String;

  d : TJSONData;
begin
  with TSoundLite do
  begin
    if aSrc.Find(JSON_CODEC_TYPE, d) then
      aCodecType := d.AsInteger;
    if aSrc.Find(JSON_FILE_NAME, d) then
      aFileName := d.AsString;
    if ((aCodecType and $fffff0) = 0) or
       SameStr(aFileName, FILENAME_GENERIC) then
    begin

    end else
      Inherited Create(aCodecType, aFileName);

    if aSrc.Find(JSON_FREQUENCY, d) then
      FFreq := d.AsInteger;
    if aSrc.Find(JSON_CHANNELS, d) then
      FChannels := d.AsInteger;
    if aSrc.Find(JSON_BITRATE, d) then
      FBitrate := d.AsInteger;
    if aSrc.Find(JSON_BITDEPTH, d) then
      FSampleSize := TOGLSound.BitdepthToSampleSize(d.AsInteger);
    if aSrc.Find(JSON_TIMETOTAL, d) then
      FTotalSize := d.AsFloat;
    if aSrc.Find(JSON_VERISON, d) then
      FVersion := d.AsInteger;
    if aSrc.Find(JSON_COMMENTS, d) and (d is TJSONObject) then
      FComments := TSoundLite.NewVorbisComment(TJSONObject(d)) else
      FComments := TOGLSound.NewVorbisComment;
  end;
end;

destructor TPassiveSoundFileInfo.Destroy;
begin
  FComments := nil;
  inherited Destroy;
end;

function TPassiveSoundFileInfo.Comments : ISoundComment;
begin
  Result := FComments;
end;

function TPassiveSoundFileInfo.Stream : TStream;
begin
  Result := nil;
end;

function TPassiveSoundFileInfo.Codec : ISoundEncDec;
begin
  Result := nil;
end;

function TPassiveSoundFileInfo.Frequency : Cardinal;
begin
  Result := FFreq;
end;

function TPassiveSoundFileInfo.Bitrate : Cardinal;
begin
  Result := FBitrate;
end;

function TPassiveSoundFileInfo.Bitdepth : Cardinal;
begin
  Result := TOGLSound.SampleSizeToBitdepth(FSampleSize);
end;

function TPassiveSoundFileInfo.SampleSize : TSoundSampleSize;
begin
  Result := FSampleSize;
end;

function TPassiveSoundFileInfo.Channels : Cardinal;
begin
  Result := FChannels;
end;

function TPassiveSoundFileInfo.Version : Cardinal;
begin
  Result := FVersion;
end;

function TPassiveSoundFileInfo.TimeTotal : Double;
begin
  Result := FTotalSize;
end;

function TPassiveSoundFileInfo.SamplesTotal : Integer;
begin
  Result := Round(FTotalSize * FFreq);
end;

{ TSoundFileInfo }

constructor TSoundFileInfo.Create(aSrc : TSoundFileInfo);
begin
  FCodecType := aSrc.CodecType;
  FFileName := aSrc.FileName;
end;

constructor TSoundFileInfo.Create(aCodecType : TSoundLiteCodecType;
  const aFileName : String);
begin
  FCodecType := aCodecType;
  FFileName := aFileName;
end;

function TSoundFileInfo.SaveToJSON : TJSONObject;
begin
  with TSoundLite do
  Result := TJSONObject.Create([JSON_FILE_NAME,  FileName,
                                JSON_CODEC_TYPE, CodecType,
                                JSON_FREQUENCY,  Frequency,
                                JSON_BITRATE,    Bitrate,
                                JSON_BITDEPTH,   Bitdepth,
                                JSON_CHANNELS,   Channels,
                                JSON_TIMETOTAL,  TimeTotal,
                                JSON_VERISON,    Version,
                                JSON_COMMENTS,   GetCommentJSON(Comments)]);
end;

function TSoundFileInfo.CodecType : TSoundLiteCodecType;
begin
  Result := FCodecType;
end;

function TSoundFileInfo.FileName : String;
begin
  Result := FFileName;
end;

{ TSLTrackFile }

procedure TSLTrackFile.SeekPercent(value : Single);
var
  cur, total : Double;
begin
  if value < 0 then value := 0;
  if value > 100 then value := 100;

  total := SoundFile.Decoder.TimeTotal;
  cur := total * double(value / 100.0);
  SeekTime(cur);
end;

procedure TSLTrackFile.SeekSample(value : Integer);
begin
  SoundFile.Decoder.SampleSeek(value);
end;

procedure TSLTrackFile.SeekTime(value : Double);
begin
  SoundFile.Decoder.TimeSeek(value);
end;

function TSLTrackFile.GetDecodedSamples : Integer;
begin
  Result := SoundFile.Decoder.SampleTell;
end;

function TSLTrackFile.GetDecodedTime : Double;
begin
  Result := SoundFile.Decoder.TimeTell;
end;

function TSLTrackFile.GetTotalTime : Double;
begin
  Result := FileInfo.TimeTotal;
end;

function TSLTrackFile.GetTotalSamples : Integer;
begin
  Result := FileInfo.SamplesTotal;
end;

function TSLTrackFile.TagsCount : Integer;
begin
  if Assigned(FileInfo.Comments) then
  begin
    Result := FileInfo.Comments.TagsCount;
  end else
    Result := 0;
end;

function TSLTrackFile.Tag(index : Integer) : String;
begin
  Result := FileInfo.Comments.GetTag(index);
end;

function TSLTrackFile.GetTagValues(const aTag : String) : String;
var i, c : integer;
begin
  Result := '';
  if Assigned(FileInfo.Comments) then
  begin
    c := FileInfo.Comments.QueryCount(aTag);
    for i := 0 to c-1 do
    begin
     if i > 0 then Result := Result + #10;
     Result := Result + FileInfo.Comments.Query(aTag, i);
    end;
  end;
end;

function TSLTrackFile.GetTagValuesVar(const aTags : array of String) : String;
var i : integer;
begin
  for i := low(aTags) to high(aTags) do
  begin
    Result := GetTagValues(aTags[i]);
    if Length(Result) > 0 then Exit;
  end;
  Result := '';
end;

function TSLTrackFile.TrackNo : String;
const atags : Array [0..2] of string = ('', 'TRACK', 'TRACKNO');
begin
  atags[0] := TOGLSoundComments.Get(TOGLSoundComments.COMMENT_TRACK)^.TagID;
  Result := GetTagValuesVar(atags);
end;

function TSLTrackFile.Title : String;
begin
  Result := GetTagValues(TOGLSoundComments.Get(TOGLSoundComments.COMMENT_TITLE)^.TagID);
  if Length(Result) = 0 then
    Result := ChangeFileExt(ExtractFileName(FileInfo.FileName), '');
end;

function TSLTrackFile.Artist : String;
begin
  Result := GetTagValues(TOGLSoundComments.Get(TOGLSoundComments.COMMENT_ARTIST)^.TagID);
end;

function TSLTrackFile.Album : String;
begin
  Result := GetTagValues(TOGLSoundComments.Get(TOGLSoundComments.COMMENT_ALBUM)^.TagID);
end;

function TSLTrackFile.Genre : String;
begin
  Result := GetTagValues(TOGLSoundComments.Get(TOGLSoundComments.COMMENT_GENRE)^.TagID);
end;

function TSLTrackFile.SoundFile : TSoundFile;
begin
  if Active and (FFileInfo is TActiveSoundFileInfo) then
  begin
    Result := (FFileInfo as TActiveSoundFileInfo).SoundFile;
  end else
    raise ESLFileNotActivated.Create;
end;

procedure TSLTrackFile.SetActive(AValue : Boolean);
begin
  if FActive = AValue then Exit;

  if AValue then begin
    if FileInfo is TPassiveSoundFileInfo then
      SetFileInfo(NewActiveFileInfo(TPassiveSoundFileInfo(FileInfo)))
    else
      raise ESLCantActivateFile.Create;
  end else
  begin
    if FileInfo is TActiveSoundFileInfo then
      SetFileInfo(NewPassiveFileInfo(TActiveSoundFileInfo(FileInfo)))
    else
      raise ESLCantActivateFile.Create;
  end;

  FActive := AValue;
end;

class function TSLTrackFile.NewPassiveFileInfo(aJSON : TJSONObject
  ) : TSoundFileInfo;
begin
  Result := TPassiveSoundFileInfo.Create(aJSON);
end;

class function TSLTrackFile.NewPassiveStreamInfo(
  aCodecType : TSoundLiteCodecType; const aFileName : String; aStr : TStream
  ) : TSoundFileInfo;
begin
  Result := TPassiveSoundStreamInfo.Create(aCodecType, aFileName);
  (Result as TPassiveSoundStreamInfo).SetStream(aStr);
end;

class function TSLTrackFile.NewPassiveFileInfo(
  aCodecType : TSoundLiteCodecType; const aFileName : String) : TSoundFileInfo;
begin
  Result := TPassiveSoundFileInfo.Create(aCodecType, aFileName);
end;

class function TSLTrackFile.NewPassiveFileInfo(aActiveFI : TActiveSoundFileInfo
  ) : TSoundFileInfo;
var
  FI : TPassiveSoundFileInfo;
begin
  if SameStr(aActiveFI.FileName, TSoundLite.FILENAME_GENERIC) then
  begin
    FI := TPassiveSoundStreamInfo.Create(aActiveFI);
    TPassiveSoundStreamInfo(FI).SetStream(aActiveFI.SoundFile.DetachStream);
  end else
    FI := TPassiveSoundFileInfo.Create(aActiveFI);
  FI.SetComments(aActiveFI.Comments);
  FI.SetProps(aActiveFI.Frequency, aActiveFI.Channels,  aActiveFI.Bitrate,
              aActiveFI.Version,   aActiveFI.TimeTotal, aActiveFI.SampleSize);
  Result := FI;
end;

class function TSLTrackFile.NewActiveFileInfo(aCodecType : TSoundLiteCodecType;
  const aFileName : String) : TSoundFileInfo;
begin
  Result := TActiveSoundFileInfo.Create(aCodecType, aFileName);
end;

class function TSLTrackFile.NewActiveFileInfo(aPassivFI : TPassiveSoundFileInfo
  ) : TSoundFileInfo;
var
  FI : TActiveSoundFileInfo;
  aFile : TSoundFile;
  Flag : Boolean;
begin
  aFile := GenerateFileByCodec(aPassivFI.CodecType);
  if Assigned(aFile)  then
  begin
    if aPassivFI is TPassiveSoundStreamInfo then
      Flag := aFile.LoadFromStream(TPassiveSoundStreamInfo(aPassivFI).DetachStream)
    else
      Flag := aFile.LoadFromFile(aPassivFI.FileName, false);

    if Flag then
    begin
      FI := TActiveSoundFileInfo.Create(aPassivFI);
      FI.SetFile(aFile);
      Result := FI;
    end else
    begin
      aFile.Free;
      Result := NewErroredFileInfo(aPassivFI);
    end;
  end else
  begin
    Result := NewErroredFileInfo(aPassivFI);
  end;
end;

function TSLTrackFile.LoadFromJSON(aJSON : TJSONObject) : Boolean;
begin
  SetFileInfo(NewPassiveFileInfo(aJSON));
  FActive := false;
  Result := FFileInfo is TPassiveSoundFileInfo;
end;

function TSLTrackFile.LoadFromFile(const aFileName : String;
  doActivate : Boolean) : Boolean;
var aStr : TFileStream;
begin
  ClearFileInfo;

  if FileExists(aFileName) then
  begin
    aStr := TFileStream.Create(aFileName, fmOpenRead);
    Result := LoadFromStream(aStr, doActivate);
  end else
    Result := false;
end;

function TSLTrackFile.LoadFromStream(Str : TStream; doActivate : Boolean
  ) : Boolean;
const HEADER_BUF_SIZE = 1024; // 1 KiB
var
  Buf : Pointer;
  BufSize : Integer;
  IsOggStream : Boolean;

  Osync : IOGGSyncState;
  Ostr  : IOGGStreamState;
  Opack : IOGGPacket;
  Opage : IOGGPage;
  Obuffer : Pointer;

  aFile : TSoundFile;
  aCodecType : TSoundLiteCodecType;

  HEAD : AnsiString;
  FN : String;
begin
  Result := false;
  if not Assigned(Str) then Exit;

  try
    ClearFileInfo;

    try
      Buf := GetMem(HEADER_BUF_SIZE);
      try
        BufSize := Str.Read(Buf^, HEADER_BUF_SIZE);
        if BufSize > 8 then
        begin
          IsOggStream := false;
          try
            // check is ogg stream
            Osync := TOGG.NewSyncState;
            Opage := TOGG.NewPage;
            Obuffer := Osync.Buffer(BufSize);
            if not Assigned(Obuffer) then Exit(false);
            Move(Buf^, Obuffer^, BufSize);
            Osync.Wrote(BufSize);
            // suggest that the first page should be small (less then 1KiB)
            // in the other case - there is not ogg format
            if (Osync.PageOut(Opage) = 1) and
                (Osync.Check = 0) and
                Assigned(Opage) and
                ((Opage.Ref^.header_len > 0) or (Opage.Ref^.body_len > 0)) and
                (Opage.Packets > 0) then
            begin
              Ostr := TOGG.NewStream(Opage.SerialNo);
              Opack := TOGG.NewPacket;
              if (Ostr.PageInIgnoreErrors(Opage) = 0) and
                  Ostr.PacketOut(Opack) and
                  (Opack.Ref^.bytes > 1) and
                  (Opack.Ref^.bytes < HEADER_BUF_SIZE) then
              begin
                BufSize := Opack.Ref^.bytes;
                Move(Opack.Ref^.packet^, Buf^, BufSize);
                IsOggStream := true;
              end;
            end;
          except
            on e : EOGGException do
              IsOggStream := false;
          end;
          Str.Position := 0;  // here can be exception

          if (BufSize >= 8) then
          begin
            SetLength(HEAD, 8);
            Move(Buf^, HEAD[1], 8);

            if HEAD.Contains('RIFF') then
              aCodecType := TSoundLite.CODEC_WAV
            else
            if HEAD.Contains('FLAC') or HEAD.Contains('fLaC') then
              aCodecType := TSoundLite.CODEC_FLAC
            else
            if HEAD.Contains('OpusHead') then
              aCodecType := TSoundLite.CODEC_OPUS
            else
            if HEAD.Contains('Vorbis') or HEAD.Contains('vorbis') then
              aCodecType := TSoundLite.CODEC_VORBIS
            else
              aCodecType := TSoundLite.CODEC_UNKNOWN;
          end else
            aCodecType := TSoundLite.CODEC_UNKNOWN;

          if IsOggStream then
          begin
            // an ogg stream
            aCodecType := aCodecType or TSoundLite.CODEC_OGG_UNKNOWN;
          end;
        end else
          Exit;
      finally
        FreeMem(Buf);
      end;
    except
      // if str is not seekable
      on e : EStreamError do
      begin
         Exit;
      end;
    end;

    aFile := GenerateFileByCodec(aCodecType);

    if Assigned(aFile) then
    begin
      if Str is TFileStream then
        FN := TFileStream(Str).FileName else
        FN := TSoundLite.FILENAME_GENERIC;

      Result := aFile.LoadFromStream(Str, [sdpReadOnly]);
      if Result then
      begin
        if doActivate then
        begin
          SetFileInfo(NewActiveFileInfo(aCodecType, FN));
          with FileInfo as TActiveSoundFileInfo do
          begin
            SetFile(aFile);
          end;
          FActive := true;
        end else
        begin
          if Str Is TFileStream then
            SetFileInfo(NewPassiveFileInfo(aCodecType, FN)) else
            SetFileInfo(NewPassiveStreamInfo(aCodecType, FN, aFile.DetachStream));
          with FileInfo as TPassiveSoundFileInfo do
          begin
            SetComments(aFile.Decoder.Comments);
            SetProps(aFile.Frequency, aFile.Channels,
                                      aFile.Bitrate, aFile.Version,
                                      aFile.Decoder.TimeTotal,
                                      aFile.SampleSize);
          end;
          FActive := false;
          FreeAndNil(aFile);
        end;
      end else
        SetFileInfo(NewErroredFileInfo(aCodecType, FN));
    end else
    begin

    end;
  finally
    if not Result then Str.Free;
  end;
end;

function TSLTrackFile.ReadData(Buffer : Pointer; aFrameSize : ISoundFrameSize;
  Ptr : Pointer) : ISoundFrameSize;
begin
  if Assigned(SoundFile) then
    Result := SoundFile.ReadData(Buffer, aFrameSize, Ptr) else
    Result := TOGLSound.NewErrorFrame;
end;

procedure TSLTrackFile.Activate;
begin
  SetActive(true);
end;

procedure TSLTrackFile.StandBy;
begin
  SetActive(false);
end;

procedure TSLTrackFile.ResetToStart;
begin
  SoundFile.ResetToStart;
end;

{ TSLFile }

procedure TSLFile.ClearFileInfo;
begin
  SetFileInfo(nil);
end;

procedure TSLFile.SetFileInfo(aFileInfo : TSoundFileInfo);
begin
  if Assigned(FFileInfo) then FreeAndNil(FFileInfo);
  FFileInfo := aFileInfo;
end;

class function TSLFile.NewErroredFileInfo(aCodecType : TSoundLiteCodecType;
  const aFileName : String) : TSoundFileInfo;
begin
  Result := TErroredSoundFileInfo.Create(aCodecType, aFileName);
end;

class function TSLFile.NewErroredFileInfo(aSrc : TSoundFileInfo) : TSoundFileInfo;
begin
  Result := TErroredSoundFileInfo.Create(aSrc);
end;

class function TSLFile.GenerateFileByCodec(aCodecType : TSoundLiteCodecType
  ) : TSoundFile;
begin
  if aCodecType = TSoundLite.CODEC_OGG_FLAC then
    Result := TFLACOggFile.Create else
  if aCodecType = TSoundLite.CODEC_OGG_VORBIS then
    Result := TVorbisFile.Create else
  if aCodecType = TSoundLite.CODEC_OGG_OPUS then
    Result := TOpusFile.Create else
  if aCodecType = TSoundLite.CODEC_FLAC then
    Result := TFLACFile.Create else
  if aCodecType = TSoundLite.CODEC_WAV then
    Result := TRIFFWaveFile.Create
  else
  if aCodecType = TSoundLite.CODEC_OGG_WAV then
    Result := TOggWaveFile.Create
  else
    Result := nil;
end;

destructor TSLFile.Destroy;
begin
  ClearFileInfo;
  inherited Destroy;
end;

{ TLibNames }

constructor TLibNames.Create;
begin
  Duplicates := dupIgnore;
end;

{ TSoundLite }

class procedure TSoundLite.GetChunkSize(aSz : ISoundFrameSize;
  aProps : ISoundProps);
var
  val : Variant;
begin
  if aProps.HasProp(TSoundLite.PROP_CHUNK_SIZE) then
  begin
    val := aProps.Get(TSoundLite.PROP_CHUNK_SIZE);
    if VarIsFloat(val) or VarIsOrdinal(val) then
      aSz.SetDurationMs(val) else
      aSz.Assign(ISoundFrameSize(val));
  end else
    aSz.SetDurationMs(cDEFAULT_CHUNK_LEN_MS);
end;

class procedure TSoundLite.GetMaxFrameSize(aSz : ISoundFrameSize;
  aProps : ISoundProps);
var
  val : Variant;
begin
  if aProps.HasProp(TSoundLite.PROP_MAX_FRAME_SIZE) then
  begin
    val := aProps.Get(TSoundLite.PROP_MAX_FRAME_SIZE);
    if VarIsFloat(val) or VarIsOrdinal(val) then
      aSz.SetDurationMs(val) else
      aSz.Assign(ISoundFrameSize(val));
  end else
    aSz.SetDurationMs(cDEFAULT_MAX_LEN_MS);
end;

class procedure TSoundLite.SetLibPath(const aDLLPath : UTF8String;
  aComponents : TSoundLiteComponents);
var i : TSoundLiteComponent;
begin
  for i := Low(TSoundLiteComponent) to High(TSoundLiteComponent) do
    if i in aComponents then
      vDLLPath[i] := aDLLPath;
end;

class procedure TSoundLite.SetLibNames(const aDLLNames : array of String;
  aIgnoreDefault : Boolean; aComponent : TSoundLiteComponent);
var i : integer;
begin
  if not Assigned(vDLLNames[aComponent]) then
  begin
    vDLLNames[aComponent] := TLibNames.Create;
    vDLLNames[aComponent].IgnoreDefaults := aIgnoreDefault;
    for i := Low(aDLLNames) to High(aDLLNames) do
    begin
      vDLLNames[aComponent].Add(aDLLNames[i]);
    end;
  end;
end;

class function TSoundLite.InitSoundLite(const aDLLPath : UTF8String;
  aComponents : TSoundLiteComponents) : Boolean;
begin
  SetLibPath(aDLLPath, aComponents);
  Result := InitSoundLite(aComponents);
end;

class function TSoundLite.InitSoundLite(const aDLLPath : UTF8String) : Boolean;
begin
  Result := InitSoundLite(aDLLPath, cALL_SL_Components);
end;

class function TSoundLite.InitSoundLite(aComponents : TSoundLiteComponents) : Boolean;
type
  SLibs = Array [0..High(byte)] of String;
  pSLibs = ^SLibs;

var i : TSoundLiteComponent;
    k : integer;
    S : Array of String;
    LibFiles : pSLibs;
    LibFilesCnt : Integer;
    usedefs, f : Boolean;
begin
  Result := true;
  for i := Low(TSoundLiteComponent) to High(TSoundLiteComponent) do
  begin
    if i in aComponents then
    begin
      if Assigned(vDLLNames[i]) then
        usedefs := not vDLLNames[i].IgnoreDefaults
      else
        usedefs := true;

      if usedefs then
      begin
        case i of
        slcOpenAL: begin
         LibFiles := @(OpenALsoftDLL);
         LibFilesCnt := Length(OpenALsoftDLL);
        end;
        slcOGG : begin
         LibFiles := @(OGGDLL);
         LibFilesCnt := Length(OGGDLL);
        end;
        slcFLAC: begin
         LibFiles := @(FLACDLL);
         LibFilesCnt := Length(FLACDLL);
        end;
        slcOpus : begin
         LibFiles := @(OpusDLL);
         LibFilesCnt := Length(OpusDLL);
        end;
        slcVorbis : begin
         LibFiles := @(VorbisDLL);
         LibFilesCnt := Length(VorbisDLL);
        end;
        else
          LibFiles := nil;
          LibFilesCnt := 0;
        end;
        if not Assigned(vDLLNames[i]) then
          vDLLNames[i] := TLibNames.Create;
        for k := 0 to LibFilesCnt-1 do
          vDLLNames[i].Add(LibFiles^[k]);
      end;

      SetLength(S, vDLLNames[i].Count);
      for k := 0 to vDLLNames[i].Count-1 do
        S[k] := vDLLPath[i] + vDLLNames[i][k];

      case i of
      slcOpenAL :
        f := TOpenAL.OALLibsLoad(S);
      slcOGG :
        f := TOGG.OGGLibsLoad(S);
      slcFLAC :
        f := TFLAC.FLACLibsLoad(S);
      slcOpus :
        f := TOpus.OpusLibsLoad(S);
      slcVorbis :
        f := TVorbis.VorbisLibsLoad(S);
      else
        f := false;
      end;
      if f then
        Include(vLibsLoaded, i);

      Result := Result and f;
    end;
  end;
end;

class function TSoundLite.InitSoundLite : Boolean;
begin
  Result := InitSoundLite(cALL_SL_Components);
end;

class function TSoundLite.Loaded(aComponent : TSoundLiteComponent) : Boolean;
begin
  Result := aComponent in vLibsLoaded;
end;

class function TSoundLite.Loaded(aComponents : TSoundLiteComponents) : Boolean;
begin
  Result := (aComponents <= vLibsLoaded);
end;

class procedure TSoundLite.DoneSoundLite;
var i : TSoundLiteComponent;
begin
  for i := Low(TSoundLiteComponent) to High(TSoundLiteComponent) do
  begin
    if i in vLibsLoaded then
    begin
      case i of
      slcOpenAL:
        TOpenAL.OALLibsUnLoad;
      slcOGG :
        TOGG.OGGLibsUnLoad;
      slcFLAC:
        TFLAC.FLACLibsUnLoad;
      slcOpus :
        TOpus.OpusLibsUnLoad;
      slcVorbis :
        TVorbis.VorbisLibsUnLoad;
      end;
      Exclude(vLibsLoaded, i);
    end;
  end;
end;

class function TSoundLite.IsCodecOgg(aCodec : TSoundLiteCodecType) : Boolean;
begin
  Result := (aCodec and CODEC_OGG_UNKNOWN) = CODEC_OGG_UNKNOWN;
end;

class function TSoundLite.CodecName(aCodec : TSoundLiteCodecType) : String;
begin
  if aCodec = CODEC_FLAC then        Result := 'FLAC Native' else
  if aCodec = CODEC_OPUS then        Result := 'Opus Native' else
  if aCodec = CODEC_VORBIS then      Result := 'Vorbis Native' else
  if aCodec = CODEC_WAV then         Result := 'WAV Native' else
  if aCodec = CODEC_OGG_FLAC then    Result := 'FLAC OGG' else
  if aCodec = CODEC_OGG_OPUS then    Result := 'Opus OGG' else
  if aCodec = CODEC_OGG_VORBIS then  Result := 'Vorbis OGG' else
  if aCodec = CODEC_OGG_WAV then     Result := 'WAV OGG' else
  if (aCodec and CODEC_OGG_UNKNOWN) = CODEC_OGG_UNKNOWN then
    Result := 'Unknown OGG' else
    Result := 'Unknown';
end;

class function TSoundLite.CodecNameShrt(aCodec : TSoundLiteCodecType) : String;
begin
  if aCodec = CODEC_FLAC then        Result := 'FLAC' else
  if aCodec = CODEC_OPUS then        Result := 'Opus' else
  if aCodec = CODEC_VORBIS then      Result := 'Vorbis' else
  if aCodec = CODEC_WAV then         Result := 'WAV' else
  if aCodec = CODEC_OGG_FLAC then    Result := 'FLACOGG' else
  if aCodec = CODEC_OGG_OPUS then    Result := 'OpusOGG' else
  if aCodec = CODEC_OGG_VORBIS then  Result := 'VorbisOGG' else
  if aCodec = CODEC_OGG_WAV then     Result := 'WAVOGG' else
  if (aCodec and CODEC_OGG_UNKNOWN) = CODEC_OGG_UNKNOWN then
    Result := 'UNKOGG' else
    Result := 'UNK';
end;

class function TSoundLite.EncoderVersionString(aCodec : TSoundLiteCodecType
  ) : String;
begin
  case aCodec of
  CODEC_OGG_FLAC, CODEC_FLAC:        Result := TFLAC.EncoderVersionString;
  CODEC_OGG_OPUS, CODEC_OPUS :       Result := TOpus.EncoderVersionString;
  CODEC_OGG_VORBIS, CODEC_VORBIS :   Result := TVorbis.EncoderVersionString;
  CODEC_OGG_WAV, CODEC_WAV :         Result := TWAVE.EncoderVersionString;
  else
    Result := '';
  end;
end;

class function TSoundLite.GetFileExt(aCodec : TSoundLiteCodecType) : String;
begin
  case aCodec of
  CODEC_FLAC :      Result := '.flac';
  CODEC_OPUS,
  CODEC_OGG_OPUS:   Result := '.opus';
  CODEC_VORBIS:     Result := '.vorbis';
  CODEC_WAV:        Result := '.wav';
  CODEC_OGG_FLAC,
  CODEC_OGG_VORBIS: Result := '.ogg';
  else
    if (aCodec and CODEC_OGG_UNKNOWN) = CODEC_OGG_UNKNOWN then
      Result := '.oga' else
      Result := '.raw';
  end;
end;

class function TSoundLite.TryGetCodecByFileName(const FN : String
  ) : TSoundLiteCodecType;
var
  ext : String;
begin
  ext := LowerCase(ExtractFileExt(FN));
  if SameStr(ext, GetFileExt(CODEC_FLAC)) then
    Exit(CODEC_FLAC) else
  if SameStr(ext, GetFileExt(CODEC_OGG_VORBIS)) then
    Exit(CODEC_OGG_VORBIS) else
  if SameStr(ext, GetFileExt(CODEC_OGG_OPUS)) then
    Exit(CODEC_OGG_OPUS) else
  if SameStr(ext, GetFileExt(CODEC_WAV)) then
    Exit(CODEC_WAV) else
    Result := CODEC_UNKNOWN;
end;

class function TSoundLite.TimeToStr(t : Double) : String;
var
  h, m, s : Integer;
  p : String;
begin
  s := Round(t);
  h := s div 3600;
  m := s mod 3600 div 60;
  s := s mod 60;
  p := Inttostr(s);
  if Length(p)<2 then p := '0' + p;
  Result := ':' + p;
  p := Inttostr(m);
  if (h > 0) and (Length(p)<2) then p := '0' + p;
  Result := p + Result;
  if h > 0 then
  begin
    Result := Inttostr(h) + ':' + Result;
  end;
end;

class function TSoundLite.NewVorbisComment(aJSON : TJSONObject
  ) : IVorbisComment;
var
  i : integer;
  ja : TJSONArray;
  t, v : TJSONData;
begin
  Result := TOGLSound.NewVorbisComment;

  with TSoundLite do
  with Result do
  begin
    Vendor := aJSON.Get(JSON_VENDOR, '');
    if aJSON.Find(JSON_TAGS, ja) then
    begin
      for i := 0 to ja.Count-1 do
      begin
        if ja[i] is TJSONObject then
        begin
          if TJSONObject(ja[i]).Find(JSON_TAG, t) and
             TJSONObject(ja[i]).Find(JSON_VALUE, v) then
             AddTag(t.AsString, v.AsString);
        end;
      end;
    end;
  end;
end;

class function TSoundLite.GetCommentJSON(aSrc : ISoundComment) : TJSONObject;
var
  i, j, c : integer;
  obj : TJSONObject;
  ja : TJSONArray;
  t : string;
begin
  with aSrc do
  begin
    ja := TJSONArray.Create;
    with TSoundLite do
    for i := 0 to TagsCount-1 do
    begin
      t := GetTag(i);
      c := QueryCount(t);
      for j := 0 to c-1 do
      begin
        obj := TJSONObject.Create([JSON_TAG, t,
                                   JSON_VALUE, Query(t, j)]);
        ja.Add(obj);
      end;
    end;
    Result := TJSONObject.Create([JSON_VENDOR, Vendor,
                                  JSON_TAGS,   ja]);
  end;
end;

class function TSoundLite.NewEncoderComment(aVorbisSrc : IVorbisComment;
  aConvertTo : TSoundLiteCodecType) : ISoundComment;
begin
  with TSoundLite, TOGLSoundComments do
  if Assigned(aVorbisSrc) then
  begin
    aVorbisSrc.SetTag(TagID(COMMENT_ENCODER),
                     Format('OGLSoundLite ver.%s / %s - %s',
                     [c_SOUND_LITE_VERSION_STRING, CodecName(aConvertTo),
                      EncoderVersionString(aConvertTo)]));
  end else
    Exit(nil);

  case aConvertTo of
  CODEC_OGG_FLAC, CODEC_FLAC:
    Result := aVorbisSrc;
  CODEC_OGG_OPUS:
    Result := TOpus.NewEncComment(aVorbisSrc);
  CODEC_OGG_VORBIS:
    Result := TVorbis.NewComment(aVorbisSrc);
  CODEC_WAV, CODEC_OGG_WAV:
    Result := TWAVE.NewComment(aVorbisSrc);
  else
    Result := nil;
  end;
end;

class function TSoundLite.NewStreamEncoder(aCodec : TSoundLiteCodecType;
  aStream : TStream; aProps : ISoundEncoderProps) : ISoundStreamEncoder;
begin
  Result := NewStreamEncoder(aCodec, aStream, [sdpForceNotSeekable, sdpWriteOnly], aProps, nil);
end;

class function TSoundLite.NewStreamEncoder(aCodec : TSoundLiteCodecType;
  aStream : TStream; aDataLimits : TSoundDataLimits;
  aProps : ISoundEncoderProps;
  aComments : ISoundComment) : ISoundStreamEncoder;
begin
  case aCodec of
  CODEC_OPUS:
    Result := TOpus.NewStreamEncoder(aStream, aProps);
  CODEC_FLAC:
    Result := TFLAC.NewStreamEncoder(aStream, aDataLimits, aProps, aComments);
  else
    Result := nil;
  end;
end;

class function TSoundLite.NewSpeexResampler(aOutBufferSize : ISoundFrameSize;
  aInRate : Cardinal; aQuality : Integer; aProps : ISoundProps ) : ISoundResampler;
begin
  Result := TSoundSpeexResampler.Create(aOutBufferSize,aInRate,aQuality, aProps);
end;

class function TSoundLite.NewSpeexResampler(aProps : ISoundProps
  ) : ISoundResampler;
begin
  Result := TSoundSpeexResampler.Create(aProps);
end;

class function TSoundLite.NewForwardFFT(aSampleCount, aChannels : Cardinal
  ) : ISoundForwardTransformation;
begin
  Result := TSoundForwardFFT.Create(aSampleCount, aChannels);
end;

class function TSoundLite.NewBackwardFFT(aSampleCount, aChannels : Cardinal
  ) : ISoundBackwardTransformation;
begin
  Result := TSoundBackwardFFT.Create(aSampleCount, aChannels);
end;

var i : TSoundLiteComponent;

initialization
  vLibsLoaded := [];
  for i := Low(TSoundLiteComponent) to High(TSoundLiteComponent) do
  begin
    vDLLPath[i] := '';
    vDLLNames[i] := nil;
  end;

finalization
  for i := Low(TSoundLiteComponent) to High(TSoundLiteComponent) do
  begin
    if Assigned(vDLLNames[i]) then
      FreeAndNil(vDLLNames[i]);
  end;
end.


