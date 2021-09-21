unit Wall.View.Image;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Ani, FMX.Effects, FMX.Layouts, System.Threading;

type
  TFrameImage = class(TFrame)
    LayoutClient: TLayout;
    Image: TImage;
    Rectangle1: TRectangle;
    RectangleBG: TRectangle;
    RectAnimationImage: TRectAnimation;
    procedure LayoutClientClick(Sender: TObject);
  private
    FTask: ITask;
    FLoading: Boolean;
    FOnClick: TNotifyEvent;
    FUrl: string;
    procedure AfterLoad;
    procedure SetOnClick(const Value: TNotifyEvent);
    procedure SetUrl(const Value: string);
  public
    procedure LoadImage(const Url: string);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OnClick: TNotifyEvent read FOnClick write SetOnClick;
    property Url: string read FUrl write SetUrl;
  end;

function Get(URL: string): TMemoryStream;

implementation

uses
  System.Net.HttpClient;

{$R *.fmx}

function Get(URL: string): TMemoryStream;
var
  HTTP: THTTPClient;
begin
  Result := TMemoryStream.Create;
  if URL.IsEmpty then
    Exit;
  HTTP := THTTPClient.Create;
  HTTP.HandleRedirects := True;
  try
    try
      if (HTTP.Get(URL, Result).StatusCode = 200) and (Result.Size > 0) then
        Result.Position := 0;
    finally
      HTTP.Free;
    end;
  except
    //
  end;
end;

{ TFrameImage }

procedure TFrameImage.AfterLoad;
begin
  Image.Height := LayoutClient.Height;
  Image.Width := LayoutClient.Height * (Image.Bitmap.Width / Image.Bitmap.Height);
  if Image.Width < LayoutClient.Width then
  begin
    Image.Width := LayoutClient.Width;
    Image.Height := LayoutClient.Width * (Image.Bitmap.Height / Image.Bitmap.Width);
  end;
  TAnimator.AnimateFloat(Image, 'Opacity', 1);
  TAnimator.AnimateFloat(RectangleBG, 'Opacity', 0, 2);
end;

constructor TFrameImage.Create(AOwner: TComponent);
begin
  inherited;
  FTask := nil;
  FLoading := False;
  Image.Opacity := 0;
end;

destructor TFrameImage.Destroy;
begin
  if FLoading and Assigned(FTask) then
    FTask.Cancel;
  inherited;
end;

procedure TFrameImage.LayoutClientClick(Sender: TObject);
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TFrameImage.LoadImage(const Url: string);
begin
  FLoading := True;
  FTask := TTask.Run(
    procedure
    var
      Mem: TMemoryStream;
    begin
      Mem := Get(Url);
      try
        if Mem.Size > 0 then
        begin
          if TTask.CurrentTask.Status <> TTaskStatus.Canceled then
            TThread.Synchronize(nil,
              procedure
              begin
                try
                  FLoading := False;
                  Image.Bitmap.LoadFromStream(Mem);
                  AfterLoad;
                except
                  //
                end;
              end);
        end;
      finally
        Mem.Free;
      end;
    end);
end;

procedure TFrameImage.SetOnClick(const Value: TNotifyEvent);
begin
  FOnClick := Value;
end;

procedure TFrameImage.SetUrl(const Value: string);
begin
  FUrl := Value;
end;

end.

