unit Wall.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  Wall.AlphaCoders.API, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,
  FMX.Layouts, FMX.Ani, Wall.View.Image, FMX.Effects, FMX.Filter.Effects,
  System.Threading, REST.Handler;

type
  TFormMain = class(TForm)
    ButtonLoadRandom: TButton;
    LayoutListClient: TLayout;
    LayoutHead: TLayout;
    VertScrollBoxItems: TVertScrollBox;
    TimerRecalcPos: TTimer;
    LayoutItems: TLayout;
    LayoutListBottom: TLayout;
    ButtonMore: TButton;
    LayoutPreview: TLayout;
    RectanglePreviewBG: TRectangle;
    ImagePreview: TImage;
    FloatAnimationPreviewBGOp: TFloatAnimation;
    ImagePreviewLoad: TImage;
    GaussianBlurEffectLoad: TGaussianBlurEffect;
    AniIndicatorLoading: TAniIndicator;
    LabelLoadError: TLabel;
    LayoutListOffset: TLayout;
    LayoutClientOver: TLayout;
    Rectangle1: TRectangle;
    TimerHeadDelay: TTimer;
    AniIndicatorLoadMore: TAniIndicator;
    AniIndicatorLoad: TAniIndicator;
    Layout1: TLayout;
    procedure FormCreate(Sender: TObject);
    procedure ButtonLoadRandomClick(Sender: TObject);
    procedure TimerRecalcPosTimer(Sender: TObject);
    procedure LayoutListClientResized(Sender: TObject);
    procedure ButtonMoreClick(Sender: TObject);
    procedure LayoutPreviewClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure VertScrollBoxItemsViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean);
    procedure TimerHeadDelayTimer(Sender: TObject);
  private
    FTask: ITask;
    FLoading: Boolean;
    FAPI: TWallpapersAPI;
    FOffset: Integer;
    Params: TParams;
    LastOldPos, LastNewPos: TPointF;
    procedure ShowHead;
    procedure HideHead;
    procedure AfterLoad;
    procedure SetAPI(const Value: TWallpapersAPI);
    procedure AppendImages(Items: TArray<TWallpaperItem>);
    procedure ClearImages;
    procedure LoadImages;
    procedure FOnImageClick(Sender: TObject);
    procedure LoadPreview(const Url: string);
    procedure AfterLoadError;
  public
    property API: TWallpapersAPI read FAPI write SetAPI;
  end;

const
  OffsetY = 5;
  OffsetX = 5;
  ItemWidth = 300;
  ItemHeight = 160;

var
  FormMain: TFormMain;

implementation

uses
  System.Math, FMX.Canvas.D2D;

{$R *.fmx}

procedure TFormMain.AfterLoad;
begin
  AniIndicatorLoading.Enabled := False;
  AniIndicatorLoading.Visible := False;
  TAnimator.AnimateFloat(ImagePreview, 'Opacity', 1);
  TAnimator.AnimateFloat(ImagePreviewLoad, 'Opacity', 0, 2);
  LabelLoadError.Visible := False;
end;

procedure TFormMain.AfterLoadError;
begin
  AniIndicatorLoading.Enabled := False;
  AniIndicatorLoading.Visible := False;
  LabelLoadError.Visible := True;
  //TAnimator.AnimateFloat(ImagePreview, 'Opacity', 1);
  //TAnimator.AnimateFloat(ImagePreviewLoad, 'Opacity', 0);
end;

procedure TFormMain.LoadPreview(const Url: string);
begin
  if FLoading and Assigned(FTask) then
    FTask.Cancel;
  FLoading := True;
  LabelLoadError.Visible := False;
  FTask := TTask.Run(
    procedure
    var
      Mem: TMemoryStream;
      Succ: Boolean;
      SelfTask: ITask;
    begin
      SelfTask := TTask.CurrentTask;
      //TCustomCanvasD2D.SharedDevice.Flush;
      Succ := False;
      try
        Mem := Get(Url);
        try
          if Mem.Size > 0 then
          begin
            if SelfTask.Status <> TTaskStatus.Canceled then
              TThread.Synchronize(nil,
                procedure
                begin
                  if SelfTask.Status <> TTaskStatus.Canceled then
                  try
                    ImagePreview.Bitmap.LoadFromStream(Mem);
                    AfterLoad;
                    Succ := True;
                  except
                    Succ := False;
                  end;
                end);
          end;
        finally
          Mem.Free;
        end;
      except
        Succ := False;
      end;
      FLoading := False;
      if (SelfTask.Status <> TTaskStatus.Canceled) and (not Succ) then
        AfterLoadError;
    end);
end;

procedure TFormMain.AppendImages(Items: TArray<TWallpaperItem>);
var
  Frame: TFrameImage;
begin
  for var Item in Items do
  begin
    Frame := TFrameImage.Create(Self);
    Frame.DisableDisappear := True;
    with Frame do
    begin
      Name := '';
      Parent := LayoutItems;
      Width := ItemWidth;
      Height := ItemHeight;
      OnClick := FOnImageClick;
      Url := Item.UrlImage;
      LoadImage(Item.UrlThumb);
      RectangleBG.Fill.Color := TAlphaColorRec.Null; // TAlphaColorF.Create(Random(128) / 255, Random(128) / 255, Random(128) / 255, 1).ToAlphaColor;
      Position.Y := LayoutItems.Height;
      Position.X := Random(Round(LayoutItems.Width));
    end;
  end;
  TimerRecalcPosTimer(TimerRecalcPos);
end;

procedure TFormMain.LoadImages;
var
  Items: TWallpapers;
begin
  Params.Add('offset', FOffset);
  if FAPI.Random(Items, Params) then
  try
    if Items.Success then
      TThread.Synchronize(nil,
        procedure
        begin
          AppendImages(Items.Wallpapers);
        end);
  finally
    Items.Free;
  end;
end;

procedure TFormMain.ButtonLoadRandomClick(Sender: TObject);
begin
  ClearImages;
  FOffset := 0;
  AniIndicatorLoad.Visible := True;
  AniIndicatorLoad.Enabled := True;
  ButtonLoadRandom.Enabled := False;
  TTask.Run(
    procedure
    begin
      try
        LoadImages;
      finally
        TThread.Synchronize(nil,
          procedure
          begin
            AniIndicatorLoad.Visible := False;
            AniIndicatorLoad.Enabled := False;
            ButtonLoadRandom.Enabled := True;
            ButtonMore.Visible := True;
          end);
      end;
    end);
end;

procedure TFormMain.ButtonMoreClick(Sender: TObject);
begin
  Inc(FOffset, 30);
  AniIndicatorLoadMore.Visible := True;
  AniIndicatorLoadMore.Enabled := True;
  ButtonMore.Enabled := False;
  TTask.Run(
    procedure
    begin
      try
        LoadImages;
      finally
        TThread.Synchronize(nil,
          procedure
          begin
            AniIndicatorLoadMore.Visible := False;
            AniIndicatorLoadMore.Enabled := False;
            ButtonMore.Enabled := True;
          end);
      end;
    end);
end;

procedure TFormMain.ClearImages;
begin
  LayoutItems.BeginUpdate;
  try
    while LayoutItems.ControlsCount > 0 do
      LayoutItems.Controls[0].Free;
  finally
    LayoutItems.EndUpdate;
  end;
end;

procedure TFormMain.FOnImageClick(Sender: TObject);
var
  Frame: TFrameImage absolute Sender;
begin
  LayoutPreview.Opacity := 0;
  LayoutPreview.Visible := True;
  TAnimator.AnimateFloat(LayoutPreview, 'Opacity', 1);
  AniIndicatorLoading.Enabled := True;
  AniIndicatorLoading.Visible := True;
  RectanglePreviewBG.Opacity := 0;
  ImagePreviewLoad.Opacity := 0;
  ImagePreview.Opacity := 0;
  TAnimator.AnimateFloat(ImagePreviewLoad, 'Opacity', 1);
  ImagePreviewLoad.Bitmap.Assign(Frame.Image.Bitmap);
  ImagePreview.Bitmap.Assign(nil);
  LoadPreview(Frame.Url);

  FloatAnimationPreviewBGOp.Enabled := False;
  FloatAnimationPreviewBGOp.Enabled := True;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FAPI := TWallpapersAPI.Create(Self);
  FAPI.Token := '46f2a217fd526109ee156ce87b667eb5';
  VertScrollBoxItems.AniCalculations.Animation := True;
  ButtonMore.Visible := False;
  LayoutPreview.Visible := False;
  //Расставим всё без анимации
  TimerRecalcPosTimer(nil);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  if FLoading and Assigned(FTask) then
    FTask.Cancel;
end;

procedure TFormMain.LayoutListClientResized(Sender: TObject);
begin
  //Сброс для перестановки
  TimerRecalcPos.Enabled := False;
  TimerRecalcPos.Enabled := True;
end;

procedure TFormMain.LayoutPreviewClick(Sender: TObject);
begin
  TAnimator.AnimateFloatWait(LayoutPreview, 'Opacity', 0);
  LayoutPreview.Visible := False;
end;

procedure TFormMain.SetAPI(const Value: TWallpapersAPI);
begin
  FAPI := Value;
end;

procedure TFormMain.TimerHeadDelayTimer(Sender: TObject);
begin
  TimerHeadDelay.Enabled := False;
  if LastNewPos.Y > LastOldPos.Y then
  begin
    if LayoutHead.Tag = 0 then
    begin
      LayoutHead.Tag := 1;
      HideHead;
    end;
  end;
  if LastNewPos.Y < LastOldPos.Y then
  begin
    if LayoutHead.Tag <> 0 then
    begin
      LayoutHead.Tag := 0;
      ShowHead;
    end;
  end;
end;

procedure TFormMain.TimerRecalcPosTimer(Sender: TObject);
var
  i, C, R, WCount, HCount: Integer;
  NC, NR: Single;
  Control: TFrameImage;
  BorderOffset: Single;
begin
  //Расстановка объектов по сетке. С анимацией, если Sender <> nil
  TimerRecalcPos.Enabled := False;
  WCount := Max(1, Trunc(LayoutItems.Width / (ItemWidth + OffsetX * 2)));
  if LayoutItems.ControlsCount > 0 then
    HCount := Ceil(LayoutItems.ControlsCount / WCount)
  else
    HCount := 0;
  BorderOffset := (LayoutItems.Width - (WCount * (ItemWidth + OffsetX * 2))) / 2;
  LayoutItems.Height := (ItemHeight + OffsetX * 2) * HCount + OffsetX;
  //TAnimator.AnimateFloat(LayoutItems, 'Height', (ItemHeight + OffsetX * 2) * HCount + OffsetX, 3, TAnimationType.InOut, TInterpolationType.Back);
  VertScrollBoxItems.RecalcSize;

  for i := 0 to Pred(LayoutItems.ControlsCount) do
    if LayoutItems.Controls[i] is TFrameImage then
    begin
      if WCount > 0 then
      begin
        Control := LayoutItems.Controls[i] as TFrameImage;
        R := i div WCount;
        C := i mod WCount;
        NC := C * (Control.Width + OffsetX * 2) + BorderOffset + OffsetX;
        NR := R * (Control.Height + OffsetY * 2) + OffsetY;
        if (Control.Position.X <> NC) or (Control.Position.Y <> NR) then
        begin
          if Sender <> nil then
          begin
            TAnimator.AnimateFloat(Control, 'Position.X', NC, 1, TAnimationType.out, TInterpolationType.Back);
            TAnimator.AnimateFloat(Control, 'Position.Y', NR, 1, TAnimationType.out, TInterpolationType.Back);
          end
          else
          begin
            Control.Position.X := NC;
            Control.Position.Y := NR;
          end;
        end;
      end;
    end;
end;

procedure TFormMain.ShowHead;
begin
  TAnimator.AnimateFloat(LayoutHead, 'Margins.Top', 0, 1, TAnimationType.InOut, TInterpolationType.Circular);
end;

procedure TFormMain.HideHead;
begin
  TAnimator.AnimateFloat(LayoutHead, 'Margins.Top', -LayoutHead.Height, 1, TAnimationType.InOut, TInterpolationType.Circular);
end;

procedure TFormMain.VertScrollBoxItemsViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean);
begin
  LastOldPos := OldViewportPosition;
  LastNewPos := NewViewportPosition;
  TimerHeadDelay.Enabled := False;
  TimerHeadDelay.Enabled := True;

  Caption := NewViewportPosition.y.ToString;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.

