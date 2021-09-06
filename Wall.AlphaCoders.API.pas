unit Wall.AlphaCoders.API;

interface

uses
  System.Classes, REST.Handler;

type
  TResponseData = class
  private
    FSuccess: Boolean;
  public
    constructor Create;
    property Success: Boolean read FSuccess write FSuccess;
  end;

  TWallpaperItem = class
  private
    FFile_size: integer;
    FFile_type: string;
    FHeight: integer;
    FId: string;
    FUrl_image: string;
    FUrl_page: string;
    FUrl_thumb: string;
    FWidth: integer;
  public
    property FileSize: integer read FFile_size write FFile_size;
    property FileType: string read FFile_type write FFile_type;
    property Height: integer read FHeight write FHeight;
    property Id: string read FId write FId;
    property UrlImage: string read FUrl_image write FUrl_image;
    property UrlPage: string read FUrl_page write FUrl_page;
    property UrlThumb: string read FUrl_thumb write FUrl_thumb;
    property Width: integer read FWidth write FWidth;
  end;

  TWallpapers = class(TResponseData)
  private
    FWallpapers: TArray<TWallpaperItem>;
  public
    property Wallpapers: TArray<TWallpaperItem> read FWallpapers;
    destructor Destroy; override;
  end;

  TWallpapersAPI = class(TComponent)
    const
      EndPointUrl = 'https://wall.alphacoders.com/api2.0/get.php';
  private
    FToken: string;
    FHandler: TRESTHandler;
    procedure SetToken(const Value: string);
    procedure SetHandler(const Value: TRESTHandler);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Newest(out Items: TWallpapers; Params: TParams = []): Boolean;
    function Random(out Items: TWallpapers; Params: TParams = []): Boolean;
    property Handler: TRESTHandler read FHandler write SetHandler;
  published
    property Token: string read FToken write SetToken;
  end;

implementation

uses
  HGM.ArrayHelper;

{ TWallpapersAPI }

constructor TWallpapersAPI.Create(AOwner: TComponent);
begin
  inherited;
  FHandler := TRESTHandler.Create;
  FHandler.Client.BaseURL := EndPointUrl;
end;

destructor TWallpapersAPI.Destroy;
begin
  FHandler.Free;
  inherited;
end;

function TWallpapersAPI.Newest(out Items: TWallpapers; Params: TParams): Boolean;
begin
  Result := Handler.Execute('', [['method', 'newest']] + Params).GetValue(Items);
end;

function TWallpapersAPI.Random(out Items: TWallpapers; Params: TParams): Boolean;
begin
  Result := Handler.Execute('', [['method', 'random']] + Params).GetValue(Items);
end;

procedure TWallpapersAPI.SetHandler(const Value: TRESTHandler);
begin
  FHandler := Value;
end;

procedure TWallpapersAPI.SetToken(const Value: string);
begin
  FToken := Value;
  FHandler.Client.AddParameter('auth', FToken);
end;

{ TResponseData }

constructor TResponseData.Create;
begin
  //
end;

{ TWallpapers }

destructor TWallpapers.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TWallpaperItem>(FWallpapers);
  inherited;
end;

end.

