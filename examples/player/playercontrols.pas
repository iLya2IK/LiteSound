unit playercontrols;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, Grids, Graphics,
  LCLType, LCLIntf, Themes,
  OGLSoundLite;

type

  TOnTrackSelect = procedure (Sender : TObject; aRowTrack : Integer) of object;

  { TPlayListGrid }

  TPlayListGrid = class(TStringGrid)
  private
    FImageList     : TImageList;
    FOnTrackSelect : TOnTrackSelect;
    FPlayList      : TSLPlayList;
    FState         : TSLPlayerState;
    function GetRowTrack : Integer;
    procedure SetImageList(AValue : TImageList);
    procedure SetOnTrackSelect(AValue : TOnTrackSelect);
    procedure SetPlayList(AValue : TSLPlayList);
    procedure SetPlayState(AValue : TSLPlayerState);
    procedure SetRowTrack(AValue : Integer);
 protected
    procedure DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState); override;
    function  SelectCell(aCol,aRow: Integer): boolean; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure UpdatePlayList;

    procedure DeleteTrack(aRowTrack : Integer);
    procedure ClearTracks;

    property ImageList : TImageList read FImageList write SetImageList;
    property PlayList : TSLPlayList read FPlayList write SetPlayList;
    property PlayState : TSLPlayerState read FState write SetPlayState;
    property RowTrack : Integer read GetRowTrack write SetRowTrack;
    property OnTrackSelect : TOnTrackSelect read FOnTrackSelect write SetOnTrackSelect;
  end;

  TOnSelectTime = procedure (Sender : TObject; aValue : Double) of object;

  { TProgressControl }

  TProgressControl = class(TCustomControl)
  private
    FCurTime, FTotalTime, FDecodedTime, FSelectTime : Double;
    FOnSelected : TOnSelectTime;
    FSelection : Boolean;
    FBarRect : TRect;
    procedure SetCurTime(AValue : Double);
    procedure SetDecodedTime(AValue : Double);
    procedure SetOnSelected(AValue : TOnSelectTime);
    procedure SetTotalTime(AValue : Double);
    procedure UpdateBarRect;
    procedure DoSelect;
  protected
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseLeave; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure EraseBackground({%H-}DC: HDC); override;
    procedure Paint; override;

    procedure UpdateProgress(CT, TT, DT : Double);

    property CurTime : Double read FCurTime write SetCurTime;
    property TotalTime : Double read FTotalTime write SetTotalTime;
    property DecodedTime : Double read FDecodedTime write SetDecodedTime;
    property OnSelected : TOnSelectTime read FOnSelected write SetOnSelected;
  end;

  { TCommentRec }

  TCommentRec = class
  private
    FTag : String;
    FValue : String;
    procedure SetTag(AValue : String);
    procedure SetValue(AValue : String);
  public
    constructor Create(const aTag, aValue : String);
    property Tag : String read FTag write SetTag;
    property Value : String read FValue write SetValue;
  end;

  { TMetaView }

  TMetaView = class(TTreeView)
  protected
    function GetIndent : Integer;
    function GetScrolledLeft : Integer;

    function IsCustomDrawn(Target: TCustomDrawTarget;
      Stage: TCustomDrawStage): Boolean; override;
    function CustomDrawItem(Node: TTreeNode; State: TCustomDrawState;
      Stage: TCustomDrawStage; var PaintImages: Boolean): Boolean; override;

    procedure Delete(Node: TTreeNode); override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;

    function AddComment(const aTagName, aTagValue: string) : TTreeNode;
  end;

resourcestring
  SArtistAlbum = 'Artist-album';
  STrackNo     = 'Track no';
  STitle       = 'Title';
  STotalTime   = 'Duration';

implementation


{ TCommentRec }

procedure TCommentRec.SetTag(AValue : String);
begin
  if FTag = AValue then Exit;
  FTag := AValue;
end;

procedure TCommentRec.SetValue(AValue : String);
begin
  if FValue = AValue then Exit;
  FValue := AValue;
end;

constructor TCommentRec.Create(const aTag, aValue : String);
begin
  FTag := aTag;
  FValue := aValue;
end;


{ TMetaView }

function TMetaView.CustomDrawItem(Node : TTreeNode; State : TCustomDrawState;
  Stage : TCustomDrawStage; var PaintImages : Boolean) : Boolean;
var
  VertMid, VertDelta, RealIndent: integer;
  NodeRect : TRect;

procedure DrawBackground(ARect: TRect);
var
  bclr: TColor;
begin
  bclr:=Canvas.Brush.Color;
  try
    Canvas.Brush.Color := clWhite;
    Canvas.FillRect(ARect);
  finally
    Canvas.Brush.Color := bclr;
  end;
end;

function SColor(C : TColor) : TColor; inline;
begin
  if (C = clDefault) or (C = clNone) then
     Result := clBlack else
     Result := C;
end;

procedure DrawNodeText(IsSelected: Boolean; NodeRect: TRect;
                             const S : String);
begin
  if IsSelected then
  begin
    Canvas.Brush.Color := $000000;
    Canvas.FillRect(NodeRect);
  end;

  NodeRect.Right := NodeRect.Left + Canvas.TextWidth(S);

  DrawText(Canvas.Handle, PChar(S), -1, NodeRect, DT_LEFT or
                                                     DT_VCENTER or
                                                     DT_SINGLELINE or
                                                     DT_NOPREFIX);
end;

procedure DrawNodeTextSimple(IsSelected: Boolean; NodeRect: TRect;
                             const S : String);
begin
  if IsSelected then
     Canvas.Font.Color := clWhite else
     Canvas.Font.Color := clBlack;

  DrawNodeText(IsSelected, NodeRect, S);
end;

procedure DrawNodeComment(IsSelected: Boolean; NodeRect: TRect;
                             commRec : TCommentRec);
var
  VRect : TRect;
begin
  Canvas.Font.Style := [fsBold];
  if IsSelected then
     Canvas.Font.Color := clWhite else
     Canvas.Font.Color := clBlack;

  VRect := NodeRect;
  NodeRect.Right := NodeRect.Right * 1 div 3;
  VRect.Left  := NodeRect.Right;
  VRect.Right := VRect.Right - 4;

  DrawNodeText(IsSelected, NodeRect, commRec.Tag);

  Canvas.Font.Style := [];

  DrawNodeText(IsSelected, VRect, commRec.Value);
end;

procedure DrawVertLine(X, Y1, Y2: Integer);
begin
  if Y1 > Y2 then
    Exit;
  if TreeLinePenStyle = psPattern then
  begin
    Y1 := Y1 + VertDelta;
    while Y1 < Y2 do
    begin
      Canvas.Pixels[X, Y1] := TreeLineColor;
      inc(Y1, 2);
    end;
  end
  else
  begin
    Canvas.MoveTo(X, Y1);
    Canvas.LineTo(X, Y2);
  end;
end;

procedure DrawHorzLine(Y, X1, X2: Integer);
begin
  if X1 > X2 then
    Exit;
  if TreeLinePenStyle = psPattern then
  begin
    while X1 < X2 do
    begin
      Canvas.Pixels[X1, Y] := TreeLineColor;
      inc(X1, 2);
    end;
  end
  else
  begin
    Canvas.MoveTo(X1, Y);
    Canvas.LineTo(X2, Y);
  end;
end;

function DrawTreeLines(CurNode: TTreeNode): integer;
// paints tree lines, returns indent
var
  CurMid: integer;
begin
  if (CurNode <> nil) and ((tvoShowRoot in Options) or (CurNode.Parent<>nil)) then
  begin
    Result := DrawTreeLines(CurNode.Parent);
    CurMid := Result + (RealIndent shr 1);
    if CurNode = Node then
    begin
      // draw horizontal line
      DrawHorzLine(VertMid, CurMid, Result + RealIndent);
    end;

    if (CurNode.GetNextVisibleSibling <> nil) then
    begin
      // draw vertical line to next brother
      if (Node.Parent = nil) and (Node.GetPrevSibling = nil) then
        DrawVertLine(CurMid, VertMid + VertDelta, NodeRect.Bottom)
      else
        DrawVertLine(CurMid, NodeRect.Top, NodeRect.Bottom);
    end else
    if (CurNode = Node) then
    begin
      // draw vertical line from top to horizontal line
      DrawVertLine(CurMid, NodeRect.Top, VertMid);
    end;
    inc(Result, RealIndent);
  end else
  begin
    Result := BorderWidth - GetScrolledLeft;
    if CurNode <> nil then // indent first level of tree with ShowRoot = false a bit
      inc(Result, RealIndent shr 2);
  end;
end;

var commRec : TCommentRec;
begin
  commRec := TCommentRec(Node.Data);
  if Stage = cdPrePaint then
  begin
    Result := false;

    NodeRect := Node.DisplayRect(false);
    VertMid := NodeRect.Top + (NodeRect.Bottom - NodeRect.Top) div 2;
    VertDelta := Ord(DefaultItemHeight and 3 = 2);
    RealIndent := Indent;

    DrawBackground(NodeRect);
    // draw tree lines
    Canvas.Pen.Color := TreeLineColor;
    Canvas.Pen.Style := TreeLinePenStyle;
    NodeRect.Left := DrawTreeLines(Node);
    Canvas.Pen.Style := psSolid;

    if assigned(commRec) then
      DrawNodeComment(Node.Selected, NodeRect, commRec)
    else
    begin
      if Node.Parent = nil then
        Canvas.Font.Style := [fsBold] else
        Canvas.Font.Style := [];
      DrawNodeTextSimple(Node.Selected, NodeRect, Node.Text);
    end;
  end else
    Result := true;
end;

procedure TMetaView.Delete(Node : TTreeNode);
begin
  if Assigned(Node.Data) then
   Node.FreeAllNodeData;
end;

procedure TMetaView.Resize;
begin
  inherited Resize;
  Invalidate;
end;

constructor TMetaView.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ReadOnly := True;
  RowSelect := True;
  Options := [tvoAutoItemHeight, tvoHideSelection, tvoKeepCollapsedNodes,
              tvoReadOnly, tvoRowSelect, tvoShowButtons, tvoShowLines,
              tvoShowRoot, tvoToolTips, tvoThemedDraw];
end;

function TMetaView.AddComment(const aTagName, aTagValue : string) : TTreeNode;
begin
  Result := Items.AddChild(nil, aTagName + ': ' + aTagValue);
  Result.Data := TCommentRec.Create(aTagName, aTagValue);
end;

function TMetaView.GetIndent : Integer;
begin
  Result := inherited Indent;
end;

function TMetaView.GetScrolledLeft : Integer;
begin
  Result := inherited ScrolledLeft;
end;

function TMetaView.IsCustomDrawn(Target : TCustomDrawTarget;
  Stage : TCustomDrawStage) : Boolean;
begin
  if Stage = cdPrePaint then begin
    if Target = dtItem then
      Result := True
    else if Target = dtControl then
      Result := True
    else
      Result := False;
  end else begin
    if Target = dtItem then
      Result := True
    else if Target = dtControl then
      Result := True
    else
      Result := False;
  end;
end;


{ TProgressControl }

procedure TProgressControl.SetOnSelected(AValue : TOnSelectTime);
begin
  if FOnSelected = AValue then Exit;
  FOnSelected := AValue;
end;

procedure TProgressControl.SetCurTime(AValue : Double);
begin
  if FCurTime = AValue then Exit;
  FCurTime := AValue;
  Invalidate;
end;

procedure TProgressControl.SetDecodedTime(AValue : Double);
begin
  if FDecodedTime = AValue then Exit;
  FDecodedTime := AValue;
  Invalidate;
end;

procedure TProgressControl.SetTotalTime(AValue : Double);
begin
  if FTotalTime = AValue then Exit;
  FTotalTime := AValue;
  Invalidate;
end;

procedure TProgressControl.UpdateBarRect;
var
  r : TRect;
  S : String;
begin
  r := Rect(0,0,Width,Height);
  S := TSoundLite.TimeToStr(36000);
  DrawText(Canvas.Handle, pchar(S), Length(S), r, DT_LEFT or DT_CALCRECT);

  FBarRect := Rect(r.Width + 8, Height div 2-4,
                   Width - r.Width - 8,Height div 2+4);
end;

procedure TProgressControl.DoSelect;
begin
  if FSelection then
  begin
    FSelection := false;
    if Enabled and Assigned(FOnSelected) then
      FOnSelected(Self, FSelectTime);
    FSelectTime := 0;
  end;
end;

procedure TProgressControl.MouseDown(Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Enabled and PtInRect(FBarRect, Point(X,Height div 2)) and (FBarRect.Width > 0) then
  begin
    FSelection := True;
    FSelectTime := Double(X - FBarRect.Left) / Double(FBarRect.Width) * FTotalTime;
    Invalidate;
  end;
end;

procedure TProgressControl.MouseMove(Shift : TShiftState; X, Y : Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if Enabled and FSelection and (ssLeft in Shift) then
  begin
    if PtInRect(FBarRect, Point(X,Height div 2)) and (FBarRect.Width > 0) then
    begin
      FSelectTime := Double(X - FBarRect.Left) / Double(FBarRect.Width) * FTotalTime;
      Invalidate;
    end;
  end;
end;

procedure TProgressControl.MouseUp(Button : TMouseButton; Shift : TShiftState;
  X, Y : Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  DoSelect;
end;

procedure TProgressControl.MouseLeave;
begin
  inherited MouseLeave;
  DoSelect;
end;

procedure TProgressControl.Resize;
begin
  inherited Resize;
  UpdateBarRect;
end;

constructor TProgressControl.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered:=true;

  FCurTime := 0;
  FTotalTime := 0;
  FDecodedTime := 0;
  FSelectTime := 0;
  FSelection := false;
  FOnSelected := nil;

  if Assigned(AOwner) and (AOwner is TGraphicControl) then
    Color := TGraphicControl(AOwner).Color else
    Color := clBtnFace;
end;

procedure TProgressControl.EraseBackground(DC : HDC);
begin
  //inherited EraseBackground(DC);
end;

procedure TProgressControl.Paint;
var
  S  : String;
  r : TRect;
  p, dp, sp : integer;
  maxt : Double;

  details: TThemedElementDetails;
begin
  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Color;
    Pen.Style := psClear;
    Rectangle(0,0,Width,Height);

    r := Rect(4,0,Width,Height);
    S := TSoundLite.TimeToStr(FCurTime);
    DrawText(Canvas.Handle, pchar(S), Length(S), r, DT_LEFT or DT_VCENTER or DT_SINGLELINE);
    r := Rect(0,0,Width-4,Height);
    S := TSoundLite.TimeToStr(FTotalTime);
    DrawText(Canvas.Handle, pchar(S), Length(S), r, DT_RIGHT or DT_VCENTER or DT_SINGLELINE);

    if FBarRect.Width > 0 then
    begin
      Brush.Style := bsSolid;
      Brush.Color := clBlack;
      Pen.Style := psClear;

      r := FBarRect;
      InflateRect(r, 4, 4);
      Details := ThemeServices.GetElementDetails(tbPushButtonPressed);
      ThemeServices.DrawElement(Canvas.Handle, Details, r, nil);

      maxt := FTotalTime;
      if maxt < FDecodedTime then maxt := FDecodedTime;

      if maxt > 0 then
      begin
        p := Round(FCurTime / maxt * FBarRect.Width);
        dp := Round(FDecodedTime / maxt * FBarRect.Width);

        r := FBarRect;
        r.Right := dp + r.Left;
        Brush.Color := clBtnShadow;
        Rectangle(r);

        r.Right := p + r.Left;
        Brush.Color := clHighlight;
        Rectangle(r);

        r.Top := 2;
        r.Bottom := Height-2;
        r.Left := FBarRect.Left + p - 10;
        r.Right := FBarRect.Left + p;
        Details := ThemeServices.GetElementDetails(tbPushButtonNormal);
        ThemeServices.DrawElement(Canvas.Handle, Details, r, nil);

        if FSelection then
        begin
          sp := Round(FSelectTime / maxt * FBarRect.Width);
          r.Top := 2;
          r.Bottom := Height-2;
          r.Left := FBarRect.Left + sp - 5;
          r.Right := FBarRect.Left + sp + 5;
          Details := ThemeServices.GetElementDetails(tbPushButtonHot);
          ThemeServices.DrawElement(Canvas.Handle, Details, r, nil);
        end;
      end;
    end;
  end;
  inherited Paint;
end;

procedure TProgressControl.UpdateProgress(CT, TT, DT : Double);
begin
  FCurTime := CT;
  FTotalTime := TT;
  FDecodedTime := DT;
  if FSelectTime > FTotalTime then FSelectTime := FTotalTime;

  Invalidate;
end;

{ TPlayListGrid }

procedure TPlayListGrid.SetImageList(AValue : TImageList);
begin
  if FImageList = AValue then Exit;
  FImageList := AValue;
  Invalidate;
end;

function TPlayListGrid.GetRowTrack : Integer;
begin
  Result := Row - 1;
end;

procedure TPlayListGrid.DrawCell(aCol, aRow : Integer; aRect : TRect;
  aState : TGridDrawState);
var
  pen : TPen;
  br  : TBrush;
  fnt : TFont;
  ts : TTextStyle;
  S  : String;
  r  : TRect;

procedure DrawPic(imi : integer);
var
  B : TBitmap;
begin
  if imi >= 0 then begin
    B := TBitmap.Create;
    try
      B.Width := 16;
      B.Height := 16;
      FImageList.Getbitmap(imi, B);
      Canvas.Draw(aRect.Left + aRect.Width div 2 - 8,
                                aRect.Top + aRect.Height div 2 - 8, B);
    finally
      B.Free;
    end;
  end;
end;

var imi : integer;
begin
  if not(CsDesigning in ComponentState) then begin
    PrepareCanvas(aCol, aRow, aState);

    pen := Canvas.Pen;
    br := Canvas.Brush;
    fnt := Canvas.Font;

    br.Color := clWhite;
    if gdFixed in aState then br.Color := $DDDDDD;
    Pen.Style := psClear;
    fnt.Color := clBlack;
    if gdSelected in aState then
    begin
      br.Color := clBlack;
      fnt.Color := clWhite;
      pen.Color := clYellow;
      pen.Style := psDot;
    end;
    Canvas.Rectangle(aRect);
    if aCol = 0 then
    begin
      if Assigned(FPlaylist) and Assigned(FImageList) then
      begin
        if ((aRow - 1) = FPlaylist.PlayPosition) and (aRow > 0) then
        begin
          case FState of
            slsPlaying : imi := 0;
            slsStopped : imi := 1;
            slsPaused  : imi := 2;
          else
            imi := -1;
          end;
          if imi >= 0 then
            DrawPic(imi);
        end;
      end;
    end else
    begin
      ts := Canvas.TextStyle;
      ts.Alignment := taLeftJustify;
      ts.Layout := tlCenter;
      ts.Wordbreak := True;
      ts.SingleLine := false;
      ts.Opaque := false;
      fnt.Style := [];
      r := aRect;
      S := Cells[aCol, aRow];
      DrawText(Canvas.Handle, pchar(s), Length(S), r, DT_LEFT or DT_WORDBREAK or DT_CALCRECT);
      if r.Height > aRect.Height then ts.Layout := tlTop;
      Canvas.TextRect(aRect, aRect.Left+2, aRect.Top + 2, S, ts);
    end;
  end else
    inherited DrawCell(aCol, aRow, aRect, aState);
end;

function TPlayListGrid.SelectCell(aCol, aRow : Integer) : boolean;
begin
  Result := inherited SelectCell(aCol, aRow);
  if Result and (aRow > 0) and Assigned(FOnTrackSelect) then
    FOnTrackSelect(Self, aRow - 1);
end;

procedure TPlayListGrid.Resize;
var
  w : Integer;
begin
  if ColCount = 5 then
  begin
    w := Width - GetSystemMetrics(SM_CXVSCROLL) - 24;
    ColWidths[0] := 24;
    ColWidths[1] := 128 * w div 336;
    ColWidths[2] := 32  * w div 336;
    ColWidths[3] := 128 * w div 336;
    ColWidths[4] := 48  * w div 336;
  end;
end;

procedure TPlayListGrid.SetOnTrackSelect(AValue : TOnTrackSelect);
begin
  if FOnTrackSelect = AValue then Exit;
  FOnTrackSelect := AValue;
end;

procedure TPlayListGrid.SetPlayList(AValue : TSLPlayList);
begin
  if FPlaylist = AValue then Exit;
  FPlaylist := AValue;
  Invalidate;
end;

procedure TPlayListGrid.SetPlayState(AValue : TSLPlayerState);
begin
  if FState = AValue then Exit;
  FState := AValue;
end;

procedure TPlayListGrid.SetRowTrack(AValue : Integer);
begin
  if (AValue >= 0) and (AValue < (RowCount-1)) then
    Row := AValue + 1;
end;

constructor TPlayListGrid.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Options := [goVertLine,goHorzLine,goRowSelect,goSmoothScroll];
  ColCount := 5;
  RowCount := 1;
  FixedCols := 0;
  FImageList := nil;
  FPlaylist := nil;
  GridLineStyle := psClear;

  FState := slsInvalid;

  Cells[1, 0] := SArtistAlbum;
  Cells[2, 0] := STrackNo;
  Cells[3, 0] := STitle;
  Cells[4, 0] := STotalTime;
end;

procedure TPlayListGrid.UpdatePlayList;
var i : integer;
    T : TSLTrackFile;
begin
  FPlayList.Lock;
  try
    BeginUpdate;
    try
      RowCount := 1 + FPlaylist.Count;

      for i := 0 to FPlaylist.Count-1 do
      begin
        T := FPlaylist[i];
        Cells[1, i+1] := T.Artist + '-' + T.Album;
        Cells[2, i+1] := T.TrackNo;
        Cells[3, i+1] := T.Title;
        Cells[4, i+1] := TSoundLite.TimeToStr(T.GetTotalTime);
      end;
    finally
      EndUpdate;
    end;
  finally
    FPlayList.UnLock;
  end;
end;

procedure TPlayListGrid.DeleteTrack(aRowTrack : Integer);
begin
  if (aRowTrack < (RowCount-1)) and (aRowTrack >= 0) then
  begin
    FPlaylist.Delete(aRowTrack);
    UpdatePlayList;
  end;
end;

procedure TPlayListGrid.ClearTracks;
begin
  FPlaylist.Clear;
  UpdatePlayList;
end;

end.

