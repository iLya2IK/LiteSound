{
   Sectre example - part of liteSound_ilya2ik

   Copyright 2023 Ilya Medvedkov

   This example demonstrates the operation of a simple speex resampler.
}

unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  ExtCtrls, OGLFastList, ECommonObjs,
  OpenGLContext, GL, GLext,
  OGLSoundLite, OGLSoundUtils, OGLSoundUtilTypes, OGLSoundDataConverting;

type

  TVector4f = Array [0..3] of Single;
  TVector3f = Array [0..2] of Single;
  TVector2f = Array [0..1] of Single;

  TVertex =  packed record
    x,  y, s0, t0 : Single;
  end;

  PPFloat = ^PSingle;

  { TGLShader }

  TGLShader = class
  private
    FVShader, FFShader, FProgram : GLuint;
    FLinkStatus : Integer;
    FOnApply    : TNotifyEvent;
  public
    procedure Init(const vShaderStr, fShaderStr : String);
    function  Uniform(const aParam : AnsiString) : GLenum;
    procedure Bind;
    procedure UnBind;
    procedure Done;

    property OnApply : TNotifyEvent read FOnApply write FOnApply;
    property Handle : GLuint read FProgram;
  end;

  { TGLFramebuffer }

  TGLFramebuffer = class
  private
    FFBO : GLenum;
    FTex : GLenum;
  public
    procedure Init(aWidth, aHeight : Cardinal);
    procedure Bind;
    procedure UnBind;
    procedure Done;

    property Handle : GLenum read FFBO;
    property TexHandle : GLenum read FTex;
  end;

  { TChunkedBuffer }

  TChunkedBuffer = class
  private
    FBuffer : PPFloat;
    FChannels : Cardinal;
    FOffset, FSize : Integer;
    FIntSize : Integer;
  public
    constructor Create(aBuffer : PPointer; aChannels : Cardinal; aOffset, aSize,
      aIntSize : Integer);
    destructor Destroy; override;

    property Buffer : PPFloat read FBuffer;
    property Size   : Integer read FSize;
    property Offset : Integer read FOffset;
    property IntSize   : Integer read FIntSize;
  end;

  { TChunkedBuffers }

  TChunkedBuffers = class(specialize TThreadSafeFastBaseSeq<TChunkedBuffer>)
  private
    FBuffer     : Pointer;
    FBufferSize : Integer;
    FCurBuffer  : TChunkedBuffer;
    FIntTotal   : Integer;
    FExtOffset  : Integer;
    FExtTotal   : Integer;
    FMaxSize    : Integer;
    FExtSSize   : Integer;
    function GetSampleOffset : Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure PushNext(aBuffer : Pointer; aSize : ISoundFrameSize;
      aResampler : ISoundResampler; aFFT : ISoundForwardTransformation);
    procedure ExtractNext;
    procedure Reset(aCurOffset : Integer);
    procedure Synchronize(aCurOffset : Integer);
    function  ReadSpectre(aDest : PPFloat; aDur : Integer) : Integer;

    property  SampleOffset : Integer read GetSampleOffset;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    PlayButton : TButton;
    FileNameEdit1 : TFileNameEdit;
    Label1 : TLabel;
    Timer1 : TTimer;
    Timer2 : TTimer;
    procedure FormClose(Sender : TObject; var CloseAction : TCloseAction);
    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure PlayButtonClick(Sender : TObject);
    procedure Timer1Timer(Sender : TObject);
    procedure Timer2Timer(Sender : TObject);
  private
    FOpenGLV : TOpenGLControl;

    shader : Array [0..1] of TGLShader;
    pass   : Array [0..1] of TGLFramebuffer;
    spectreTex  : Array [0..1] of GLenum;
    spectreData : PPFloat;
    vboQuad, vboIndQuad, vaoQuad : GLenum;
    vboQuadArr : Array [0..3] of TVertex;
    vboQuadInd : Array [0..3] of Word;
    aspect : Array [0..1] of single;

    targ : integer;
    startstamp : QWord;

    FGLInitialized, FDrawingNow : Boolean;

    FBuffers : TChunkedBuffers;
    FPlayerThread  : TSLPlayerThread;
    FPlayer  : TSLPlayer;

    FResampler : ISoundResampler;
    FTransform : ISoundForwardTransformation;

    procedure OpenGLControl1Paint(Sender: TObject);
    procedure FadeSpectre;
    procedure InitSpectre;
    procedure WorkPass(Sender : TObject);
    procedure ColorPass(Sender : TObject);

    procedure OnStartPlay(Sender : TSLCustomPlayer);
    procedure OnNextData(aBuffer : Pointer; aFrame : ISoundFrameSize);
  public

  end;

procedure CheckOpenGLError(ignore : Boolean);

var
  Form1 : TForm1;

implementation

const SPECTRE_RES  = 256;
      SPECTRE_SAMPLES = SPECTRE_RES * 2;
      SPECTRE_FREQ = 16000;

{$ifdef Windows}
const sLibsPath = '..\libs\';
{$endif}

{$R *.lfm}

{ TChunkedBuffer }

constructor TChunkedBuffer.Create(aBuffer : PPointer;
  aChannels : Cardinal;
  aOffset, aSize,
  aIntSize : Integer);
begin
  FBuffer   := PPFloat(aBuffer);
  FSize     := aSize;
  FOffset   := aOffset;
  FChannels := aChannels;
  FIntSize  := aIntSize;
end;

destructor TChunkedBuffer.Destroy;
var
  i : integer;
begin
  for i := 0 to FChannels-1 do
    FreeMem(FBuffer[i]);
  FreeMem(FBuffer);
  inherited Destroy;
end;

{ TChunkedBuffers }

function TChunkedBuffers.GetSampleOffset : Integer;
begin
  Lock;
  try
    if Assigned(FCurBuffer) then
      Result := FCurBuffer.Offset + FExtOffset else
      Result := -1;
  finally
    UnLock;
  end;
end;

constructor TChunkedBuffers.Create;
begin
  inherited Create;

  FBuffer := nil;
  FBufferSize := 0;
  FCurBuffer := nil;
  FExtOffset := 0;
  FIntTotal := 0;
  FExtTotal := 0;
  FExtSSize := 0;
  FMaxSize := 0;
end;

destructor TChunkedBuffers.Destroy;
begin
  Reset(0);

  inherited Destroy;
end;

procedure TChunkedBuffers.PushNext(aBuffer : Pointer; aSize : ISoundFrameSize;
  aResampler : ISoundResampler; aFFT : ISoundForwardTransformation);
var
  c : TChunkedBuffer;
  aResampled : PPFloat;
  amp : Single;
  stotal, i, j, scnt, incnt, chc, cnt : Integer;
  fft_res : TSoundComplexData;
  sz : ISoundFrameSize;
  wbuf : Pointer;
begin
  stotal  := aSize.AsSamples;
  chc := aSize.Channels;

  if FMaxSize = 0 then
  begin
    FMaxSize := Round(Double(SPECTRE_SAMPLES * aSize.Frequency) / Double(SPECTRE_FREQ));
    FExtSSize := chc * aSize.ByteDepth;
    if Assigned(FBuffer) then FreeMem(FBuffer);
    FBuffer := GetMem(FMaxSize * FExtSSize);
  end;

  scnt := 0;
  while scnt < stotal do
  begin
    incnt := FMaxSize - FBufferSize;
    if incnt <= (stotal - scnt) then
    begin
      if FBufferSize > 0 then
      begin
        cnt := FBufferSize * FExtSSize;
        Move(aBuffer^, pbyte(FBuffer)[cnt], incnt * FExtSSize);
        wbuf := FBuffer;
      end else
        wbuf := @(PByte(aBuffer)[scnt * FExtSSize]);

      sz := aSize.EmptyDuplicate;
      sz.IncSamples(FMaxSize);
      aResampler.WriteInterleave(wbuf, sz);

      FBufferSize := 0;

      // uninterleave and resample input audio data chunk
      aResampled := GetMem(SizeOf(PSingle) * chc);
      for i := 0 to chc-1 do
        aResampled[i] := GetMem(SizeOf(Single) * SPECTRE_RES);

      aFFT.ProcessInterleave(aResampler.OutBuffer, ss16bit);

      for i := 0 to chc-1 do
      begin
        for j := 0 to SPECTRE_RES-1 do
        begin
          fft_res := aFFT.OutputHarmonic(i, j);
          amp := Sqrt(Sqr(fft_res.r) +
                      Sqr(fft_res.i)) / SPECTRE_RES;
          amp := Ln(amp + 1.0) / 2.30258509299;
          aResampled[i][j] := amp;
        end;
      end;

      c := TChunkedBuffer.Create(PPointer(aResampled), chc,
                                   FExtTotal, sz.AsSamples, SPECTRE_RES);
      Push_back(c);

      Inc(FExtTotal, incnt);
      Inc(FIntTotal, SPECTRE_RES);
    end else
    begin
      incnt := (stotal - scnt);
      Move(PByte(aBuffer)[scnt * FExtSSize], FBuffer^, incnt * FExtSSize);
      FBufferSize := incnt;
    end;

    Inc(scnt, incnt);
  end;
end;

procedure TChunkedBuffers.ExtractNext;
begin
  Lock;
  try
    if Assigned(FCurBuffer) then
      FreeAndNil(FCurBuffer);

    FCurBuffer := PopValue;
  finally
    UnLock;
  end;
end;

procedure TChunkedBuffers.Reset(aCurOffset : Integer);
begin
  Lock;
  try
    if Assigned(FBuffer) then FreeMemAndNil(FBuffer);
    FBufferSize := 0;
    Clean;
    FExtTotal := 0;
    FIntTotal := 0;
    FExtSSize := 0;
    FMaxSize := 0;
    FExtOffset := aCurOffset;
    ExtractNext;
  finally
    UnLock;
  end;
end;

procedure TChunkedBuffers.Synchronize(aCurOffset : Integer);
begin
  while Assigned(FCurBuffer) and
        ((FCurBuffer.Offset + FCurBuffer.Size) < aCurOffset) do
  begin
    ExtractNext;
  end;
end;

function TChunkedBuffers.ReadSpectre(aDest : PPFloat; aDur : Integer) : Integer;
var
  sz : integer;
begin
  Lock;
  try
    Result := 0;
    if Assigned(FCurBuffer) then
    begin
      while (Result < aDur) and Assigned(FCurBuffer) do
      begin
        sz := (aDur - Result);
        if sz >= FCurBuffer.IntSize then
        begin
          Move(PSingle(FCurBuffer.Buffer[0])[0],
               PSingle(aDest[0])[Result], sz * Sizeof(Single));
          if FCurBuffer.FChannels > 1 then
          begin
            Move(PSingle(FCurBuffer.Buffer[1])[0],
                 PSingle(aDest[1])[Result], sz * Sizeof(Single));
          end else
          begin
            Move(PSingle(FCurBuffer.Buffer[0])[0],
                 PSingle(aDest[1])[Result], sz * Sizeof(Single));
          end;
          Inc(Result, sz);
          ExtractNext;
        end else
          Break;
      end;
    end else
    begin
      ExtractNext;
      if Assigned(FCurBuffer) then
       Result := ReadSpectre(aDest, aDur);
    end;
  finally
    UnLock;
  end;
end;

{ TGLFramebuffer }

procedure TGLFramebuffer.Init(aWidth, aHeight : Cardinal);
begin
  glGenFramebuffers(1, @FFBO);
  if (glCheckFramebufferStatus(GL_FRAMEBUFFER) = GL_FRAMEBUFFER_COMPLETE) then
  begin
    Bind;

    glGenTextures(1, @FTex);
    glBindTexture(GL.GL_TEXTURE_2D, FTex);
    CheckOpenGLError(false);

    glTexParameterf(GL.GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameterf(GL.GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameterf(GL.GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameterf(GL.GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

    CheckOpenGLError(false);

    glTexImage2D(GL.GL_TEXTURE_2D, 0, GL_RGBA, aWidth, aHeight, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);

    CheckOpenGLError(false);

    glBindTexture(GL.GL_TEXTURE_2D, 0);

    CheckOpenGLError(true);

    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, FTex, 0);

    CheckOpenGLError(false);
    if(glCheckFramebufferStatus(GL_FRAMEBUFFER) <> GL_FRAMEBUFFER_COMPLETE) then
    	raise Exception.Create('Framebuffer is not complete');

    UnBind;
  end;
end;

procedure TGLFramebuffer.Bind;
begin
  glBindFramebuffer(GL_FRAMEBUFFER, FFBO);
  CheckOpenGLError(false);
end;

procedure TGLFramebuffer.UnBind;
begin
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
end;

procedure TGLFramebuffer.Done;
begin
  UnBind;
  glDeleteFramebuffers(1, @FFBO);
  glDeleteTextures(1, @FTex);
end;

{ TGLShader }

procedure TGLShader.Init(const vShaderStr, fShaderStr : String);

procedure CompileShader(s : GLuint);
var
  p : Pchar;
  cs : Integer;
begin
  glCompileShader(s);
  glGetShaderiv(s, GL_COMPILE_STATUS, @cs);
  if cs = 0 then
  begin
    glGetShaderiv(s, GL_INFO_LOG_LENGTH, @cs);
    if cs > 0 then
    begin
      p := GetMem(cs);
      glGetShaderInfoLog(s, cs, @cs, p);
      try
        raise Exception.CreateFmt('Shader is not compiled. error ''%s''', [StrPas(p)]);
      finally
        freemem(p);
      end;
    end;
  end;
end;

var
  SL : TStringList;
  p : PGLchar;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(vShaderStr);

    FVShader := glCreateShader(GL_VERTEX_SHADER);
    p := PGLchar(SL.Text);
    glShaderSource(FVShader, 1, @p, nil);
    CompileShader(FVShader);

    SL.LoadFromFile(fShaderStr);

    FFShader := glCreateShader(GL_FRAGMENT_SHADER);
    p := PGLchar(SL.Text);
    glShaderSource(FFShader, 1, @p, nil);
    CompileShader(FFShader);

    FProgram := glCreateProgram();
    glAttachShader(FProgram, FVShader);
    glAttachShader(FProgram, FFShader);

    glLinkProgram(FProgram);

    glGetProgramiv(FProgram, GL_LINK_STATUS, @FLinkStatus);
    if FLinkStatus = 0 then
    begin
      raise Exception.Create('Can''t link glsl program');
    end;

    CheckOpenGLError(false);
  finally
    SL.Free;
  end;
end;

function TGLShader.Uniform(const aParam : AnsiString) : GLenum;
begin
  Result := glGetUniformLocation(FProgram, PChar(aParam));
end;

procedure TGLShader.Bind;
begin
  glUseProgram(FProgram);
  CheckOpenGLError(true);
  if Assigned(FOnApply) then
    FOnApply(Self);
end;

procedure TGLShader.UnBind;
begin
  glUseProgram(0);
end;

procedure TGLShader.Done;
begin
  glDetachShader(FProgram, FvShader);
  glDetachShader(FProgram, FfShader);
  glDeleteShader(FvShader);
  glDeleteShader(FfShader);
  glDeleteProgram(FProgram);
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender : TObject);
begin
  Randomize;
  if Load_GL_VERSION_3_3 then
  begin
    FGLInitialized := false;
    FDrawingNow := false;

    FResampler := nil;
    FTransform := nil;

    FOpenGLV := TOpenGLControl.Create(Self);
    FOpenGLV.Parent := Self;

    FOpenGLV.Top := Form1.Height div 2;
    FOpenGLV.Width := 16;
    FOpenGLV.Height := 16;
    FOpenGLV.Left := 1;
    FOpenGLV.Align := alClient;
    FOpenGLV.OpenGLMajorVersion := 3;
    FOpenGLV.OpenGLMinorVersion := 3;
    FOpenGLV.Enabled := false;

    Timer2.Interval := SPECTRE_SAMPLES * 1000 div SPECTRE_FREQ;

    spectreData := GetMem(Sizeof(PSingle) * 2);
    spectreData[0] := GetMem(SPECTRE_RES * Sizeof(Single));
    spectreData[1] := GetMem(SPECTRE_RES * Sizeof(Single));

    FOpenGLV.OnPaint := @OpenGLControl1Paint;
    targ := 0;
    startstamp := GetTickCount64;
  end else
    FOpenGLV := nil;

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
  if TSoundLite.InitSoundLite then
  begin
    FBuffers := TChunkedBuffers.Create;

    FPlayerThread := TSLPlayer.StartThread;
    FPlayer := FPlayerThread.Player;
    FPlayer.OnNextData := @OnNextData;
    FPlayer.OnStartPlay := @OnStartPlay;
  end;
end;

procedure TForm1.FormClose(Sender : TObject; var CloseAction : TCloseAction);
begin
  FOpenGLV.Enabled := false;
  Timer1.Enabled := false;
  Timer2.Enabled := false;
end;

procedure TForm1.FormDestroy(Sender : TObject);
var
  i : integer;
begin
  if Assigned(FPlayerThread) then
  begin
    FPlayerThread.Terminate;
    FPlayerThread.WaitFor;
    FreeAndNil(FPlayerThread);

    if assigned(FBuffers) then
      FreeAndNil(FBuffers);

    TSoundLite.DoneSoundLite;
  end;

  FTransform := nil;
  FResampler := nil;

  if FGLInitialized then
  begin
    FGLInitialized := false;
    for i := 0 to high(pass) do
    if assigned(pass[i]) then
    begin
      pass[i].Done;
      pass[i].Free;
    end;
    for i := 0 to high(shader) do
    if assigned(shader[i]) then
    begin
      shader[i].Done;
      shader[i].Free;
    end;
    for i := 0 to high(spectreTex) do
    if spectreTex[i] > 0 then
    begin
      glDeleteTextures(1, @(spectreTex[i]));
    end;
  end;
  if Assigned(spectreData) then
  begin
    FreeMemAndNil(spectreData[0]);
    FreeMemAndNil(spectreData[1]);
    FreeMemAndNil(spectreData);
  end;
end;

procedure TForm1.PlayButtonClick(Sender : TObject);
begin
  if FPlayer.Playing then
  begin
    PlayButton.Caption := 'Play';
    FileNameEdit1.Enabled := true;
    FPlayer.Stop;
    FPlayer.Playlist.Clear;
    FOpenGLV.Enabled := false;
    Timer1.Enabled := false;
    Timer2.Enabled := false;
  end else
  begin
    if FileExists(FileNameEdit1.FileName) then
    begin
      PlayButton.Caption := 'Stop';
      FileNameEdit1.Enabled := false;

      InitSpectre;

      FPlayer.Playlist.AddFromFile(FileNameEdit1.FileName);
      FPlayer.Playlist.PlayRepeat := true;
      FPlayer.Playlist.PlayPosition := 0;

      if Assigned(FPlayer.Playlist.CurrentTrack) then
      with FPlayer.Playlist.CurrentTrack.FileInfo do
      begin
        FResampler := TSoundLite.NewSpeexResampler(
                         TOGLSound.FrameFromDuration(SPECTRE_FREQ, Channels, SampleSize, Timer2.Interval),
                         Frequency, 6, nil);
        FTransform := TSoundLite.NewForwardFFT(SPECTRE_SAMPLES, Channels);

        FPlayer.Play;
        FOpenGLV.Enabled := true;
        Timer1.Enabled := true;
        Timer2.Enabled := true;
      end;
    end;
  end;
end;

procedure TForm1.Timer1Timer(Sender : TObject);
begin
  FOpenGLV.Repaint;
  FadeSpectre;
end;

procedure TForm1.Timer2Timer(Sender : TObject);
begin
  if FPlayer.Playing then
  begin
    FBuffers.Synchronize(FPlayer.PlayedSamples);
    FBuffers.ReadSpectre(spectreData, SPECTRE_RES);
  end;
end;

procedure TForm1.OpenGLControl1Paint(Sender: TObject);

procedure InitGL;
var i : integer;
begin
  if FGLInitialized then exit;
  FGLInitialized:=true;
  glClearDepth(1.0);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE);
  CheckOpenGLError(false);

  pass[0] := TGLFramebuffer.Create;
  pass[0].Init(512, 512);
  pass[1] := TGLFramebuffer.Create;
  pass[1].Init(512, 512);

  CheckOpenGLError(false);

  shader[0] := TGLShader.Create;
  shader[0].Init('firstpass_vs.glsl','firstpass_fs.glsl');
  shader[0].OnApply := @WorkPass;
  shader[1] := TGLShader.Create;
  shader[1].Init('colorpass_vs.glsl','colorpass_fs.glsl');
  shader[1].OnApply := @ColorPass;

  CheckOpenGLError(false);

  glGenTextures(2, @(spectreTex[0]));

  CheckOpenGLError(false);

  for i := 0 to high(spectreTex) do
  begin
    glBindTexture(GL.GL_TEXTURE_1D, spectreTex[i]);
    glTexImage1D(GL.GL_TEXTURE_1D, 0,
                 GL_RED, SPECTRE_RES, 0, GL_RED, GL_FLOAT,
                 spectreData[i]);


    glTexParameterf(GL.GL_TEXTURE_1D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameterf(GL.GL_TEXTURE_1D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameterf(GL.GL_TEXTURE_1D, GL_TEXTURE_WRAP_S, GL_REPEAT);

    CheckOpenGLError(false);
  end;

  glBindTexture(GL.GL_TEXTURE_1D, 0);

  // VERTEX 0
  vboQuadArr[0].x := -1.0;
  vboQuadArr[0].y := -1.0;
  vboQuadArr[0].s0 := 0.0;
  vboQuadArr[0].t0 := 1.0;
  // VERTEX 1
  vboQuadArr[1].x := -1.0;
  vboQuadArr[1].y := 1.0;
  vboQuadArr[1].s0 := 0.0;
  vboQuadArr[1].t0 := 0.0;
  // VERTEX 2
  vboQuadArr[2].x := 1.0;
  vboQuadArr[2].y := -1.0;
  vboQuadArr[2].s0 := 1.0;
  vboQuadArr[2].t0 := 1.0;
  // VERTEX 3
  vboQuadArr[3].x := 1.0;
  vboQuadArr[3].y := 1.0;
  vboQuadArr[3].s0 := 1.0;
  vboQuadArr[3].t0 := 0.0;


  vboQuadInd[0] := 0;
  vboQuadInd[1] := 1;
  vboQuadInd[2] := 2;
  vboQuadInd[3] := 3;

  glGenVertexArrays(1, @vaoQuad);
  glGenBuffers(1, @vboQuad);
  glGenBuffers(1, @vboIndQuad);

  glBindVertexArray(vaoQuad);

    // VBO
    glBindBuffer(GL_ARRAY_BUFFER, vboQuad);
    glBufferData(GL_ARRAY_BUFFER, sizeof(TVertex)*4, @(vboQuadArr[0]), GL_STATIC_DRAW);

    // data configuration
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(TVertex), pointer(ptrint(0)));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(TVertex), pointer(ptrint(8)));

    // EBO
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vboIndQuad);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(Word)*4, @(vboQuadInd[0]), GL_STATIC_DRAW);

  glBindVertexArray(0);

  CheckOpenGLError(true);

  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vboIndQuad);

  CheckOpenGLError(true);
end;

var
  lstShader : TGLShader;

procedure DrawQuad2d(shader : TGLShader);
begin
  if lstShader <> shader then
  begin
    if lstShader <> nil then
      lstShader.UnBind;
    if shader <> nil then
      shader.Bind;
    lstShader := shader;
  end;

  CheckOpenGLError(false);

  glBindVertexArray(vaoQuad);
  glDrawElements(GL_TRIANGLE_STRIP, 4, GL_UNSIGNED_SHORT, pointer(ptrint(0)));    // The starting point of the IBO
  glBindVertexArray(0);

  CheckOpenGLError(false);
end;

var i : integer;
begin
  if not FOpenGLV.Enabled then Exit;
  if FDrawingNow then Exit;
  FDrawingNow := true;
  if FOpenGLV.MakeCurrent then
  begin
    CheckOpenGLError(true);

    InitGL;

    glDisable(GL_DEPTH_TEST);
    glDepthMask(0);
    glClearColor(0,0,0,1);

    CheckOpenGLError(true);

    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    CheckOpenGLError(false);

    for i := 0 to high(spectreTex) do
    begin
      glBindTexture(GL.GL_TEXTURE_1D, spectreTex[i]);

      glTexSubImage1D(GL.GL_TEXTURE_1D,
                      0, 0, SPECTRE_RES,
                      GL_RED, GL_FLOAT,
                      spectreData[i]);
    end;
    glBindTexture(GL.GL_TEXTURE_1D, 0);

    targ := 1;
    CheckOpenGLError(false);
    pass[0].Bind;
    CheckOpenGLError(false);
    glViewport(0,0, 512, 512);
    CheckOpenGLError(true);
    aspect[0] := 1; aspect[1] := 1;
    DrawQuad2d(shader[1]);
    pass[1].Bind;
    glViewport(0,0, 512, 512);
    DrawQuad2d(shader[0]);
    pass[0].UnBind;
    lstShader.UnBind;
    glViewport(0,0,FOpenGLV.Width, FOpenGLV.Height);
    targ := 0;

    if FOpenGLV.Width > FOpenGLV.Height then
    begin
      aspect[0] := Single(FOpenGLV.Height) / Single(FOpenGLV.Width);
      aspect[1] := 1;
    end else
    begin
      aspect[0] := 1;
      aspect[1] := Single(FOpenGLV.Width) / Single(FOpenGLV.Height);
    end;

    DrawQuad2d(shader[1]);

    if lstShader <> nil then
      lstShader.UnBind;

    CheckOpenGLError(true);
    glFlush;
    FOpenGLV.SwapBuffers;
  end;
  FDrawingNow := false;
end;

procedure TForm1.FadeSpectre;
var
  i, j : integer;
begin
  for j := 0 to high(spectreTex) do
  begin
    for i := 0 to SPECTRE_RES-1 do
      PSingle(spectreData[j])[i] := 0.95 * PSingle(spectreData[j])[i];
  end;
end;

procedure TForm1.InitSpectre;
var
  i, j : integer;
  V : Single;
begin
  for j := 0 to high(spectreTex) do
  begin
    for i := 0 to SPECTRE_RES-1 do
    begin
      case j of
      0:
      V := (Cos(Double(random * 15.0 + i * 10.0) / 100.0) + (1.0 - random * 2.0)) * 0.5 ;
      1:
      V := (Sin(Double(random * 25.0 + i * 20.0) / 100.0) + (1.0 - random * 2.0)) * 0.5 ;
      end;
      PSingle(spectreData[j])[i] := V;
    end;
  end;
end;

procedure TForm1.WorkPass(Sender : TObject);
begin
  glActiveTexture(GL_TEXTURE0_ARB);
  glBindTexture(GL.GL_TEXTURE_2D, pass[1].TexHandle);
  glUniform1i(shader[0].Uniform('Texture0'), 0);
  glActiveTexture(GL_TEXTURE1_ARB);
  glBindTexture(GL.GL_TEXTURE_1D, spectreTex[0]);
  glUniform1i(shader[0].Uniform('LSpectre'), 1);
  glActiveTexture(GL_TEXTURE2_ARB);
  glBindTexture(GL.GL_TEXTURE_1D, spectreTex[1]);
  glUniform1i(shader[0].Uniform('RSpectre'), 2);
  glUniform1f(shader[0].Uniform('time'), Double(GetTickCount64 - startstamp) / 1000.0);

  CheckOpenGLError(true);
end;

procedure TForm1.ColorPass(Sender : TObject);
begin
  glActiveTexture(GL_TEXTURE0_ARB);
  glBindTexture(GL.GL_TEXTURE_2D, pass[targ].TexHandle);
  glUniform1i(shader[1].Uniform('Texture0'), 0);
  glUniform2f(shader[1].Uniform('aspect'), aspect[0], aspect[1]);

  CheckOpenGLError(true);
end;

procedure TForm1.OnStartPlay(Sender : TSLCustomPlayer);
begin
  FBuffers.Reset(0);
end;

procedure TForm1.OnNextData(aBuffer : Pointer; aFrame : ISoundFrameSize);
begin
  FBuffers.PushNext(aBuffer, aFrame, FResampler, FTransform);
end;

procedure CheckOpenGLError(ignore : Boolean);
var
  err : GLEnum;
begin
  while true do
  begin
    err := glGetError();
    if err = GL_NO_ERROR then
      Break
    else
    begin
      if not ignore then
        raise Exception.CreateFmt('OpenGL error %d', [err]);
    end;
  end;
end;

end.

