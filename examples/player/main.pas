{
   Player example - part of liteSound_ilya2ik

   Copyright 2023 Ilya Medvedkov

   This example emulates the operation of a simple audio player, 
   demonstrates working with comments, metadata, playlists.
}


unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, playercontrols,
  LCLType, LCLIntf, LMessages,
  OGLSoundLite, OGLSoundUtilTypes;

const
  WM_START_PLAY          = LM_USER + 1;
  WM_STOP_PLAY           = LM_USER + 2;
  WM_PAUSE               = LM_USER + 3;
  WM_RESUME              = LM_USER + 4;
  WM_TRACK_CHANGED       = LM_USER + 5;

type

  { TForm1 }

  TForm1 = class(TForm)
    CheckBox1 : TCheckBox;
    CheckBox2 : TCheckBox;
    ImageList1 : TImageList;
    Label1 : TLabel;
    Label2 : TLabel;
    SaveDialog1 : TSaveDialog;
    OpenDialog1 : TOpenDialog;
    Panel1 : TPanel;
    Panel2 : TPanel;
    Splitter1 : TSplitter;
    ToolButton1 : TToolButton;
    SavePlaylistBtn : TToolButton;
    OpenPlaylistBtn : TToolButton;
    ToolButton3 : TToolButton;
    Timer1 : TTimer;
    ToolBar1 : TToolBar;
    ToolBar2 : TToolBar;
    OpenFileBtn : TToolButton;
    PlayTrackBtn : TToolButton;
    StopTrackBtn : TToolButton;
    PauseTrackBtn : TToolButton;
    GoBackBtn : TToolButton;
    GoForwardBtn : TToolButton;
    ToolButton2 : TToolButton;
    DeleteTrackBtn : TToolButton;
    ClearPlaylistBtn : TToolButton;
    VolumeControl : TTrackBar;
    procedure CheckBox1Change(Sender : TObject);
    procedure CheckBox2Change(Sender : TObject);
    procedure ClearPlaylistBtnClick(Sender : TObject);
    procedure DeleteTrackBtnClick(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure GoBackBtnClick(Sender : TObject);
    procedure GoForwardBtnClick(Sender : TObject);
    procedure OpenFileBtnClick(Sender : TObject);
    procedure OpenPlaylistBtnClick(Sender : TObject);
    procedure PauseTrackBtnClick(Sender : TObject);
    procedure PlayTrackBtnClick(Sender : TObject);
    procedure SavePlaylistBtnClick(Sender : TObject);
    procedure StopTrackBtnClick(Sender : TObject);
    procedure Timer1Timer(Sender : TObject);
    procedure VolumeControlChange(Sender : TObject);
  private
    FPlayer : TSLPlayer;
    FPlayerThread : TSLPlayerThread;
    FPlayListGrid : TPlayListGrid;
    FMetaView     : TMetaView;
    FPlayProgress : TProgressControl;

    procedure PlayListGridDblClick(Sender : TObject);
    procedure PlayListGridSelectTrack(Sender : TObject; aRowTrack : Integer);
    procedure SelectTime(Sender : TObject; aValue : Double);

    procedure ShowComments(F : TSLTrackFile);
    procedure OnStartPlay(Sender : TSLPlayer; aTrack : TSLTrackFile);
    procedure OnPause(Sender : TSLPlayer; aTrack : TSLTrackFile);
    procedure OnResume(Sender : TSLPlayer; aTrack : TSLTrackFile);
    procedure OnTrackChanged(Sender : TSLPlayList;
                                    {%H-}aLstTrack, aNewTrack : TSLTrackFile);
    procedure OnStopPlay(Sender : TSLPlayer);

    procedure HandleStartPlay(var {%H-}Msg: TLMessage); message WM_START_PLAY;
    procedure HandleStopPlay(var {%H-}Msg: TLMessage); message WM_STOP_PLAY;
    procedure HandlePause(var {%H-}Msg: TLMessage); message WM_PAUSE;
    procedure HandleResume(var {%H-}Msg: TLMessage); message WM_RESUME;
    procedure HandleTrackChanged(var Msg: TLMessage); message WM_TRACK_CHANGED;

    procedure SetTimeLabel(aLabel : TLabel; aTime : Double);
  public
    procedure PlayCurrentTrack;
  end;

var
  Form1 : TForm1;

implementation

resourcestring
  SJSONExt    = 'Playlist JSON|*.json';
  SAudioExt   = 'Xiph audio files|*.ogg;*.oga;*.flac;*.opus;*.wav';
  SStopped    = '[Stopped]';
  SVendor     = 'Vendor';
  SCodec      = 'Codec';

{$ifdef Windows}
const sLibsPath = '..\libs\';
{$endif}

{$R *.lfm}

{ TForm1 }

procedure TForm1.OpenFileBtnClick(Sender : TObject);
var
  i : integer;
begin
  OpenDialog1.Options := [ofAllowMultiSelect, ofEnableSizing, ofViewDetail];
  OpenDialog1.Filter := SAudioExt;
  if OpenDialog1.Execute then
  begin
    for i := 0 to OpenDialog1.Files.Count-1 do
      FPlayer.Playlist.AddFromFile(OpenDialog1.Files[i]);

    FPlayListGrid.UpdatePlayList;
  end;
end;

procedure TForm1.OpenPlaylistBtnClick(Sender : TObject);
begin
  OpenDialog1.Options := [ofEnableSizing, ofViewDetail];
  OpenDialog1.Filter := SJSONExt;
  if OpenDialog1.Execute then
  begin
    FPlayer.Playlist.LoadFromFile(OpenDialog1.FileName);

    FPlayListGrid.UpdatePlayList;
  end;
end;

procedure TForm1.PauseTrackBtnClick(Sender : TObject);
begin
  FPlayer.Pause;
end;

procedure TForm1.PlayListGridDblClick(Sender : TObject);
begin
  PlayCurrentTrack;
end;

procedure TForm1.PlayListGridSelectTrack(Sender : TObject; aRowTrack : Integer);
begin
  if aRowTrack >= 0 then
  begin
    ShowComments(FPlayer.Playlist[aRowTrack]);
    if not (FPlayer.Playing or FPlayer.Paused) then
      FPlayProgress.TotalTime := FPlayer.Playlist[aRowTrack].GetTotalTime;
  end;
end;

procedure TForm1.SelectTime(Sender : TObject; aValue : Double);
begin
  FPlayer.SeekTime(aValue);
end;

procedure TForm1.PlayTrackBtnClick(Sender : TObject);
begin
  PlayCurrentTrack;
end;

procedure TForm1.SavePlaylistBtnClick(Sender : TObject);
begin
  if SaveDialog1.Execute then
    FPlayer.Playlist.SaveToFile(SaveDialog1.FileName);
end;

procedure TForm1.StopTrackBtnClick(Sender : TObject);
begin
  FPlayer.Stop;
end;

procedure TForm1.Timer1Timer(Sender : TObject);
var
  aDecodedTime, aCurTime, aTotalTime : Double;
begin
  aCurTime := FPlayer.PlayedTime;
  aDecodedTime := FPlayer.DecodedTime;
  aTotalTime := FPlayProgress.TotalTime;
  if aDecodedTime > aTotalTime then aTotalTime := aDecodedTime;
  FPlayProgress.UpdateProgress(aCurTime, aTotalTime, aDecodedTime);
end;

procedure TForm1.VolumeControlChange(Sender : TObject);
begin
  FPlayer.Gain := single(VolumeControl.Position) / single(VolumeControl.Max);
end;

procedure TForm1.ShowComments(F : TSLTrackFile);
var
  PNODE : TTreeNode;
  TC, i, j, QC : Integer;
  TN, S, TNC : String;
  C : ISoundComment;
begin
  FPlayer.FullLock;
  try
    FMetaView.BeginUpdate;
    try
      FMetaView.Items.Clear;
      C := F.FileInfo.Comments;
      if Assigned(C) then
      begin
        TC := C.TagsCount;
        for i := 0 to TC-1 do
        begin
          TN := C.GetTag(i);
          TNC := TOGLSoundComments.GetByID(TN)^.TagName;
          QC := C.QueryCount(TN);
          if QC = 1 then
          begin
            S := C.Query(TN, 0);
            fMetaView.AddComment(TNC, S);
          end else
          begin
            PNODE := fMetaView.Items.AddChild(nil, TNC);
            for j := 0 to QC-1 do
            begin
              S := C.Query(TN, j);
              fMetaView.Items.AddChild(PNODE, S);
            end;
          end;
        end;
        TN := SVendor;
        S := C.Vendor;
        if Length(S) > 0 then
          fMetaView.AddComment(TN, S);
        TN := SCodec;
        S := TSoundLite.CodecName(F.FileInfo.CodecType);
        if Length(S) > 0 then
          fMetaView.AddComment(TN, S);
        TN := STotalTime;
        S := TSoundLite.TimeToStr(F.GetTotalTime);
        if Length(S) > 0 then
          fMetaView.AddComment(TN, S);

        fMetaView.FullExpand;
      end;
    finally
      fMetaView.EndUpdate;
    end;
  finally
    FPlayer.FullUnLock;
  end;
end;

procedure TForm1.OnStartPlay(Sender : TSLPlayer; aTrack : TSLTrackFile);
begin
  PostMessage(Form1.Handle, WM_START_PLAY, PtrInt(aTrack), 0);
end;

procedure TForm1.OnPause(Sender : TSLPlayer; aTrack : TSLTrackFile);
begin
  PostMessage(Form1.Handle, WM_PAUSE, PtrInt(aTrack), 0);
end;

procedure TForm1.OnResume(Sender : TSLPlayer; aTrack : TSLTrackFile);
begin
  PostMessage(Form1.Handle, WM_RESUME, PtrInt(aTrack), 0);
end;

procedure TForm1.OnTrackChanged(Sender : TSLPlayList; aLstTrack,
  aNewTrack : TSLTrackFile);
begin
  PostMessage(Form1.Handle, WM_TRACK_CHANGED, PtrInt(aNewTrack), 0);
end;

procedure TForm1.OnStopPlay(Sender : TSLPlayer);
begin
  PostMessage(Form1.Handle, WM_STOP_PLAY, 0, 0);
end;

procedure TForm1.HandleStartPlay(var Msg : TLMessage);
begin
  Timer1.Enabled := true;
  FPlayProgress.Enabled := true;
  FPlayListGrid.PlayState := FPlayer.Status;
  FPlayListGrid.UpdatePlayList;
end;

procedure TForm1.HandleStopPlay(var Msg : TLMessage);
begin
  Timer1.Enabled := false;
  FPlayProgress.Enabled := false;
  FPlayProgress.CurTime := 0;
  FPlayProgress.DecodedTime := 0;
  FPlayListGrid.PlayState := FPlayer.Status;
  FPlayListGrid.UpdatePlayList;
end;

procedure TForm1.HandlePause(var Msg : TLMessage);
begin
  Timer1.Enabled := false;
  FPlayProgress.Enabled := true;
  FPlayListGrid.PlayState := FPlayer.Status;
  FPlayListGrid.UpdatePlayList;
end;

procedure TForm1.HandleResume(var Msg : TLMessage);
begin
  Timer1.Enabled := true;
  FPlayProgress.Enabled := true;
  FPlayListGrid.PlayState := FPlayer.Status;
  FPlayListGrid.UpdatePlayList;
end;

procedure TForm1.HandleTrackChanged(var Msg : TLMessage);
var
  aTrack : TSLTrackFile;
begin
  aTrack := TSLTrackFile(Msg.WParam);
  if Assigned(aTrack) then
  begin
    FPlayer.FullLock;
    try
      Caption := aTrack.Title;
      FPlayProgress.UpdateProgress(0, aTrack.GetTotalTime, 0);
    finally
      FPlayer.FullUnLock;
    end;
  end else
    Caption := SStopped;
  FPlayListGrid.Invalidate;
end;

procedure TForm1.SetTimeLabel(aLabel : TLabel; aTime : Double);
begin
  aLabel.Caption := TSoundLite.TimeToStr(aTime);
end;

procedure TForm1.PlayCurrentTrack;
begin
  if FPlayListGrid.RowCount > 1 then
    begin
      if FPlayListGrid.RowTrack < 0 then
        FPlayListGrid.RowTrack := 0;
      if FPlayer.Playlist.PlayPosition <> FPlayListGrid.RowTrack then
        FPlayer.Playlist.PlayPosition := FPlayListGrid.RowTrack;

      FPlayer.Play;
    end;
end;

procedure TForm1.FormCreate(Sender : TObject);
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

  FPlayerThread := TSLPlayer.StartThread;
  FPlayer := FPlayerThread.Player;
  FPlayer.OnPause := @OnPause;
  FPlayer.OnResume := @OnResume;
  FPlayer.OnStartPlay := @OnStartPlay;
  FPlayer.OnStopPlay := @OnStopPlay;
  FPlayer.Playlist.OnTrackChanged := @OnTrackChanged;

  FPlayProgress := TProgressControl.Create(Panel2);
  FPlayProgress.Parent := Panel2;
  FPlayProgress.Height := 32;
  FPlayProgress.Top := ToolBar1.Height;
  FPlayProgress.Align := alTop;
  FPlayProgress.OnSelected := @SelectTime;

  FPlayListGrid := TPlayListGrid.Create(Panel2);
  FPlayListGrid.Parent := Panel2;
  FPlayListGrid.ImageList := ImageList1;
  FPlayListGrid.PlayList := FPlayer.Playlist;
  FPlayListGrid.Align := alClient;
  FPlayListGrid.OnDblClick := @PlayListGridDblClick;
  FPlayListGrid.OnTrackSelect := @PlayListGridSelectTrack;

  FMetaView := TMetaView.Create(Panel1);
  FMetaView.Parent := Panel1;
  FMetaView.Align := alClient;
end;

procedure TForm1.CheckBox1Change(Sender : TObject);
begin
  FPlayer.Playlist.PlayRepeat := CheckBox1.Checked;
end;

procedure TForm1.CheckBox2Change(Sender : TObject);
begin
  FPlayer.Playlist.PlayShuffle := CheckBox2.Checked;
end;

procedure TForm1.ClearPlaylistBtnClick(Sender : TObject);
begin
  FPlayListGrid.ClearTracks;
end;

procedure TForm1.DeleteTrackBtnClick(Sender : TObject);
begin
  if FPlayListGrid.RowTrack >= 0 then
      FPlayListGrid.DeleteTrack(FPlayListGrid.RowTrack);
end;

procedure TForm1.FormDestroy(Sender : TObject);
begin
  FPlayerThread.Terminate;
  FPlayerThread.WaitFor;
  FPlayerThread.Free;
  TSoundLite.DoneSoundLite;
end;

procedure TForm1.GoBackBtnClick(Sender : TObject);
begin
  FPlayer.Playlist.Prev;
  FPlayer.Play;
end;

procedure TForm1.GoForwardBtnClick(Sender : TObject);
begin
  FPlayer.Playlist.Next;
  FPlayer.Play;
end;

end.

