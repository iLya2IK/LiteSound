{
   Converter example - part of liteSound_ilya2ik

   Copyright 2023 Ilya Medvedkov

   This example emulates the operation of a simple audio file converter.
}


unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  ComCtrls, ExtCtrls,
  OGLSoundLite, OGLSoundUtils, OGLSoundUtilTypes, ECommonObjs;

type

  { TForm1 }

  TForm1 = class(TForm)
    AutoExtCh : TCheckBox;
    BitrateModeCh : TComboBox;
    Button1 : TButton;
    CodecCh : TComboBox;
    EncodedFileEd : TEdit;
    Label1 : TLabel;
    Label2 : TLabel;
    Label3 : TLabel;
    Label4 : TLabel;
    Label5 : TLabel;
    Label6 : TLabel;
    Panel1 : TPanel;
    SrcFileNameEd : TFileNameEdit;
    StatusLabel : TLabel;
    ProgressBar1 : TProgressBar;
    Timer1 : TTimer;
    QualityTrack : TTrackBar;
    BitrateTrack : TTrackBar;
    procedure Button1Click(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure Timer1Timer(Sender : TObject);
  private
    FConvertThread: TSLConverterThread;
    FConvertedSamples : TThreadInteger;
    FTotalSamples : TThreadInteger;
    FIsFinished : TThreadBoolean;
  public
    procedure OnConvertProgress(Sender : TObject;
                       const aConverted, aTotal : ISoundFrameSize;
                       isFinish : Boolean);
  end;

var
  Form1 : TForm1;

implementation

const
  cInitialDir = '..' + PathDelim + 'media' + PathDelim;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender : TObject);
var
  aCodecType : TSoundLiteCodecType;
  aEncProps : ISoundEncoderProps;
  S, D : String;
begin
  if Assigned(FConvertThread) then
  begin
    if FConvertThread.Converter.Converting then Exit;
    FreeAndNil(FConvertThread);
  end;

  ProgressBar1.Position := 0;
  StatusLabel.Caption := 'Converting';
  Button1.Enabled := false;
  Panel1.Enabled := false;
  ProgressBar1.Enabled := true;
  FIsFinished.Value := false;
  FConvertedSamples.Value := 0;

  with TSoundLite do
  begin
    aEncProps := TOGLSound.EncProps([]);
    case CodecCh.ItemIndex of
      0 : aCodecType := CODEC_FLAC;
      1 : aCodecType := CODEC_OGG_FLAC;
      2 : aCodecType := CODEC_OGG_OPUS;
      3 : aCodecType := CODEC_OGG_VORBIS;
      4 : aCodecType := CODEC_WAV;
    end;

    case aCodecType of
      CODEC_FLAC, CODEC_OGG_FLAC : begin
        aEncProps.Add(ENC_PROP_FLAC_COMPR_LEVEL, QualityTrack.Position);
      end;
      CODEC_OGG_OPUS, CODEC_OGG_VORBIS : begin
        aEncProps.Add(ENC_PROP_QUALITY, Double(QualityTrack.Position) / 10.0);
        aEncProps.Add(ENC_PROP_BITRATE, BitrateTrack.Position * 1000);
        aEncProps.Add(ENC_PROP_MODE, TSoundEncoderMode(BitrateModeCh.ItemIndex));
      end;
    end;
  end;

  S :=  EncodedFileEd.Text;
  D := ExtractFileDir(S);
  if (Length(D) = 0) or SameStr(S, D) then
    S := ExtractFilePath(SrcFileNameEd.FileName) + S;
  if AutoExtCh.Checked then
    S := ChangeFileExt(S, TSoundLite.GetFileExt(aCodecType));

  FConvertThread := TSLConverterThread.Create;
  FConvertThread.Converter.OnProgress := @OnConvertProgress;
  FConvertThread.StartConvert(SrcFileNameEd.FileName,
                              S,
                              aCodecType,
                              aEncProps);

  Timer1.Enabled := true;
end;

procedure TForm1.FormCreate(Sender : TObject);
begin
  FConvertThread := nil;
  FIsFinished := TThreadBoolean.Create(false);
  FTotalSamples := TThreadInteger.Create(0);
  FConvertedSamples := TThreadInteger.Create(0);
  SrcFileNameEd.InitialDir := cInitialDir;
  TSoundLite.InitSoundLite([slcOGG, slcFLAC, slcOpus, slcVorbis]);
end;

procedure TForm1.FormDestroy(Sender : TObject);
begin
  if Assigned(FConvertThread) then
  begin
    FConvertThread.Terminate;
    FConvertThread.WaitFor;
    FreeAndNil(FConvertThread);
  end;
  FConvertedSamples.Free;
  FTotalSamples.Free;
  FIsFinished.Free;
  TSoundLite.DoneSoundLite;
end;

procedure TForm1.Timer1Timer(Sender : TObject);
begin
  if FTotalSamples.Value > 0 then
  begin
    ProgressBar1.Position := FConvertedSamples.Value * 100 div
                                                     FTotalSamples.Value;
  end;
  if FIsFinished.Value or
     (not FConvertThread.Converter.Converting) then
  begin
    Timer1.Enabled := false;
    Button1.Enabled := true;
    Panel1.Enabled := true;
    ProgressBar1.Enabled := false;

    FIsFinished.Value := false;

    StatusLabel.Caption := 'Finished';
  end;
end;

procedure TForm1.OnConvertProgress(Sender : TObject; const aConverted,
  aTotal : ISoundFrameSize; isFinish : Boolean);
begin
  FIsFinished.Value := isFinish;
  FTotalSamples.Value := aTotal.AsSamples;
  FConvertedSamples.Value := aConverted.AsSamples;
end;

end.

