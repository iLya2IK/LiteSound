{
   Recorder example - part of liteSound_ilya2ik

   Copyright 2023 Ilya Medvedkov

   This example emulates the operation of a simple audio recorder.
}


unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls,
  ExtCtrls,
  OGLSoundLite, OGLSoundUtils, OGLSoundUtilTypes,
  OGLOpenALWrapper;

type

  { TForm1 }

  TForm1 = class(TForm)
    ComboBox1 : TComboBox;
    CapturingDevicesList : TComboBox;
    Label1 : TLabel;
    Label2 : TLabel;
    Label3 : TLabel;
    Label4 : TLabel;
    Panel1 : TPanel;
    SaveToFileEdit : TFileNameEdit;
    StartStopRecordBtn : TButton;
    Timer1 : TTimer;
    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure StartStopRecordBtnClick(Sender : TObject);
    procedure Timer1Timer(Sender : TObject);
  private
    FRecoder : TSLFileRecorder;
    FComments : IVorbisComment;
    FEncProps : ISoundEncoderProps;
  public

  end;

var
  Form1 : TForm1;

implementation

resourcestring
  sStartCapture     = 'Start capturing';
  sStopCapture      = 'Stop capturing';
  sCaptureNoDevices = 'No capturing devices detected';
  sReadyToCapture   = 'Ready to capture';
  sNotReady         = 'Not ready';
  sCaptured         = 'Captured : %s';

{$ifdef Windows}
const sLibsPath = '..\libs\';
{$endif}

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender : TObject);
var
  DSL : TStringList;
begin
  {$ifdef Windows}
  TSoundLite.SetLibPath(sLibsPath, [slcOpenAL, slcOGG, slcFLAC,
                                    slcOpus, slcVorbis]);
  TSoundLite.SetLibNames(['soft_oal.dll'], true, slcOpenAL);
  TSoundLite.SetLibNames(['libogg-0.dll'], true, slcOGG);
  TSoundLite.SetLibNames(['libFLAC-8.dll'], true, slcFLAC);
  TSoundLite.SetLibNames(['libopus-0.dll',
                          'libopusenc-0.dll',
                          'libopusfile-0.dll'], true, slcOpus);
  TSoundLite.SetLibNames(['libvorbis-0.dll',
                          'libvorbisenc-2.dll',
                          'libvorbisfile-3.dll'], true, slcVorbis);
  {$endif}

  TSoundLite.InitSoundLite;

  DSL := TOpenAL.ListOfAllCapureDevices;
  if Assigned(DSL) and (DSL.Count > 0) then
  begin
    CapturingDevicesList.Items.Assign(DSL);
    CapturingDevicesList.ItemIndex := 0;

    FRecoder := TSLFileRecorder.Create;

    FComments := TOGLSound.NewVorbisComment;
    FComments.Vendor := 'SLRecorder';
    With TOGLSoundComments do
    begin
    FComments.AddTag(TagID(COMMENT_ARTIST), 'Your voice');
    FComments.AddTag(TagID(COMMENT_TITLE),  'Record');
    end;
    with TSoundLite do
    FEncProps := TOGLSound.EncProps([ENC_PROP_MODE, oemVBR,
                                     PROP_CHANNELS, 1,
                                     PROP_FREQUENCY, 48000,
                                     ENC_PROP_BITRATE, 96000,
                                     ENC_PROP_QUALITY, 0.5,
                                     PROP_SAMPLE_SIZE, ss16bit]);
    StartStopRecordBtn.Caption := sStartCapture;
    Label3.Caption := sReadyToCapture;
  end else
  begin
    Panel1.Enabled := false;
    StartStopRecordBtn.Caption := sNotReady;
    Label3.Caption := sCaptureNoDevices;
  end;

  if Assigned(DSL) then
    DSL.Free;
end;

procedure TForm1.FormDestroy(Sender : TObject);
begin
  FRecoder.Stop;
  FRecoder.Free;
  TSoundLite.DoneSoundLite;
end;

procedure TForm1.StartStopRecordBtnClick(Sender : TObject);
var
  S : String;
  aCodec : TSoundLiteCodecType;
begin
  if TSLCaptureState(FRecoder.Status) = slcsCapturing then
  begin
    Panel1.Enabled := true;
    Timer1.Enabled := false;
    FRecoder.Stop;
    Label3.Caption := sReadyToCapture;
    StartStopRecordBtn.Caption := sStartCapture;
  end else
  begin
    FRecoder.Init(CapturingDevicesList.Text, FEncProps);

    with TSoundLite do
    case ComboBox1.ItemIndex of
    0: aCodec := CODEC_FLAC;
    1: aCodec := CODEC_OGG_FLAC;
    2: aCodec := CODEC_OGG_OPUS;
    3: aCodec := CODEC_OGG_VORBIS;
    4: aCodec := CODEC_WAV;
    5: aCodec := CODEC_OGG_WAV;
    else
      aCodec := CODEC_UNKNOWN;
    end;

    S := ChangeFileExt(SaveToFileEdit.FileName, TSoundLite.GetFileExt(aCodec));
    if FRecoder.SaveToFile(S, aCodec, FEncProps, FComments) then
    begin
      FRecoder.Start;
      Timer1.Enabled := True;
      Panel1.Enabled := false;
      StartStopRecordBtn.Caption := sStopCapture;
    end;
  end;
end;

procedure TForm1.Timer1Timer(Sender : TObject);
var
  f : ISoundFrameSize;
begin
  if FRecoder.Status = slcsCapturing then
  begin
    FRecoder.Proceed;
    f := FRecoder.TotalCaptured;
    Label3.Caption := Format(sCaptured, [TSoundLite.TimeToStr(f.AsDurationSec)]);
  end;
end;

end.

