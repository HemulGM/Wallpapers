unit REST.Handler;

interface

uses
  System.Classes, System.SysUtils, FMX.Types, REST.Client, REST.Json, JSON;

type
  THandlerException = class(Exception);

  TArrayOfString = TArray<string>;

  TArrayOfStringHelper = record helper for TArrayOfString
    function ToString: string; overload; inline;
    function ToJson: string; overload; inline;
    function Add(const Value: string): Integer; inline;
    procedure Delete(const Value: string); inline;
    procedure Assign(Source: TStrings); overload;
    function IsEmpty: Boolean;
    function Length: Integer;
    function IndexOf(const Value: string): Integer;
  end;

  TArrayOfInteger = TArray<Integer>;

  TArrayOfIntegerHelper = record helper for TArrayOfInteger
    function ToString: string; overload; inline;
    function ToJson: string; overload; inline;
    function Add(Value: Integer): Integer; inline;
    procedure Delete(const Value: Integer); inline;
    function IsEmpty: Boolean;
    function Length: Integer;
    function IndexOf(const Value: Integer): Integer;
  end;

  TParam = TArray<string>;

  TParams = TArray<TParam>;

  TParamsHelper = record helper for TParams
  private
    function AddParam(var Dest: TParams; Param: TParam): Integer; inline;
  public
    function Add(Param: TParam): Integer; overload; inline;
    function Add(Key: string; Value: string): Integer; overload; inline;
    function Add(Key: string; Value: Integer): Integer; overload; inline;
    function Add(Key: string; Value: Extended): Integer; overload; inline;
    function Add(Key: string; Value: TDateTime; Format: string = ''): Integer; overload; inline;
    function Add(Key: string; Value: Boolean): Integer; overload; inline;
    function Add(Key: string; Value: TArrayOfString): Integer; overload; inline;
    function Add(Key: string; Value: TArrayOfInteger): Integer; overload; inline;
    function KeyExists(Key: string): Boolean; inline;
    function GetValue(Key: string): string; inline;
    function Remove(Key: string): string; inline;
  end;

  TResponseError = record
    Code: Integer;
    Text: string;
  end;

  TResponse = record
  private
    function AppendItemsTag(JSON: string): string; inline;
  public
    Success: Boolean;
    JSON: string;
    function GetJSONValue: TJSONValue;
    function GetValue<T: class, constructor>(const Field: string; var Value: T): Boolean; overload;
    function GetValue<T: class, constructor>(var Value: T): Boolean; overload;
    function GetValues<T: class, constructor>(var Value: T): Boolean; overload;
  end;

  TOnHandlerError = procedure(Sender: TObject; E: Exception; Code: Integer; Text: string) of object;

  TOnHandlerLog = procedure(Sender: TObject; const Value: string) of object;

  TOnHandlerExecute = function(Sender: TObject; Request: TRESTRequest): TResponse;

  TRequestConstruct = class
    class var
      Client: TRESTClient;
  public
    class function Request(Resource: string; Params: TParams): TRESTRequest;
  end;

  TRESTHandler = class
  private
    FStartRequest: Cardinal;
    FRequests: Integer;
    FRESTClient: TRESTClient;
    FOnError: TOnHandlerError;
    FOnLog: TOnHandlerLog;
    FUseServiceKeyOnly: Boolean;
    FExecuting: Integer;
    FFullLog: Boolean;
    FRequestLimitPerSecond: Integer;
    FOnExecute: TOnHandlerExecute;
    FQueueWaits: Integer;
    function FExecute(Request: TRESTRequest): TResponse;
    function GetExecuting: Boolean;
    function FOnExecuted(Request: TRESTRequest): TResponse;
    procedure Queue;
    procedure FLog(const Value: string);
    procedure SetOnError(const Value: TOnHandlerError);
    procedure SetOnLog(const Value: TOnHandlerLog);
    procedure SetUseServiceKeyOnly(const Value: Boolean);
    procedure SetFullLog(const Value: Boolean);
    procedure SetRequestLimitPerSecond(const Value: Integer);
    procedure SetOnExecute(const Value: TOnHandlerExecute);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Log(Sender: TObject; const Text: string);
    //
    function Execute(Request: string; Params: TParams): TResponse; overload;
    function Execute(Request: string; Param: TParam): TResponse; overload;
    function Execute(Request: string): TResponse; overload;
    function Execute(Request: TRESTRequest; FreeRequset: Boolean = False): TResponse; overload;
    //
    property OnError: TOnHandlerError read FOnError write SetOnError;
    property OnLog: TOnHandlerLog read FOnLog write SetOnLog;
    property OnExecute: TOnHandlerExecute read FOnExecute write SetOnExecute;
    //
    property Client: TRESTClient read FRESTClient;
    property Executing: Boolean read GetExecuting;
    property QueueWaits: Integer read FQueueWaits;
    //
    property UseServiceKeyOnly: Boolean read FUseServiceKeyOnly write SetUseServiceKeyOnly;
    property FullLog: Boolean read FFullLog write SetFullLog;
    property RequestLimitPerSecond: Integer read FRequestLimitPerSecond write SetRequestLimitPerSecond;
  end;

function BoolToString(Value: Boolean): string;

implementation

uses
  System.DateUtils, System.StrUtils;

function BoolToString(Value: Boolean): string;
begin
  Result := IfThen(Value, '1', '0');
end;

{ TRequsetConstruct }

class function TRequestConstruct.Request(Resource: string; Params: TParams): TRESTRequest;
var
  Param: TParam;
begin
  Result := TRESTRequest.Create(nil);
  Result.Client := Client;
  Result.Resource := Resource;
  Result.Response := TRESTResponse.Create(Result);
  for Param in Params do
    if (Length(Param) > 1) and (not Param[0].IsEmpty) then
      Result.Params.AddItem(Param[0], Param[1]);
end;

{ TRESTHandler }

constructor TRESTHandler.Create;
begin
  inherited;
  FExecuting := 0;
  FStartRequest := 0;
  FQueueWaits := 0;
  FRequests := 0;
  FRequestLimitPerSecond := 3;
  FRESTClient := TRESTClient.Create(nil);
  FRESTClient.Accept := 'application/json, text/plain; q=0.9, text/html;q=0.8,';
  FRESTClient.AcceptCharset := 'UTF-8, *;q=0.8';
  FRESTClient.RaiseExceptionOn500 := False;

  TRequestConstruct.Client := FRESTClient;
end;

destructor TRESTHandler.Destroy;
begin
  FRESTClient.Free;
  inherited;
end;

function TRESTHandler.Execute(Request: string; Param: TParam): TResponse;
begin
  Result := Execute(TRequestConstruct.Request(Request, [Param]), True);
end;

function TRESTHandler.Execute(Request: string; Params: TParams): TResponse;
begin
  Result := Execute(TRequestConstruct.Request(Request, Params), True);
end;

function TRESTHandler.Execute(Request: string): TResponse;
begin
  Result := Execute(TRequestConstruct.Request(Request, []), True);
end;

function TRESTHandler.Execute(Request: TRESTRequest; FreeRequset: Boolean): TResponse;
begin
  try
    Inc(FExecuting);
    Result := FExecute(Request);
  finally
    Dec(FExecuting);
    if FreeRequset then
      Request.Free;
  end;
end;

procedure TRESTHandler.Queue;

  procedure WaitTime(MS: Int64);
  begin
    if MS <= 0 then
      Exit;
    MS := TThread.GetTickCount + MS;
    while MS > TThread.GetTickCount do
      Sleep(10);
  end;

begin
  //Без очереди
  if FRequestLimitPerSecond <= 0 then
    Exit;
  Inc(FRequests);
  //Если был запрос
  if FStartRequest > 0 then
  begin
    Inc(FQueueWaits);
    //Если предел запросов
    if FRequests > FRequestLimitPerSecond then
    begin
      FRequests := 0;
      WaitTime(1000 - Int64(TThread.GetTickCount - FStartRequest));
    end;
    Dec(FQueueWaits);
  end;
  FStartRequest := TThread.GetTickCount;
end;

function TRESTHandler.FExecute(Request: TRESTRequest): TResponse;
begin
  if FFullLog then
    FLog(Request.GetFullRequestURL);

  Queue;
  Request.Execute;

  if FFullLog then
    FLog(Request.Response.JSONText);

  Result := FOnExecuted(Request);
end;

function TRESTHandler.FOnExecuted(Request: TRESTRequest): TResponse;
begin
  if Assigned(FOnExecute) then
    Result := FOnExecute(Self, Request)
  else
  begin
    Result.JSON := Request.Response.Content;
    Result.Success := True;
  end;
end;

procedure TRESTHandler.FLog(const Value: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, Value);
end;

function TRESTHandler.GetExecuting: Boolean;
begin
  Result := FExecuting > 0;
end;

procedure TRESTHandler.Log(Sender: TObject; const Text: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Sender, Text);
end;

procedure TRESTHandler.SetOnError(const Value: TOnHandlerError);
begin
  FOnError := Value;
end;

procedure TRESTHandler.SetOnExecute(const Value: TOnHandlerExecute);
begin
  FOnExecute := Value;
end;

procedure TRESTHandler.SetOnLog(const Value: TOnHandlerLog);
begin
  FOnLog := Value;
end;

procedure TRESTHandler.SetRequestLimitPerSecond(const Value: Integer);
begin
  FRequestLimitPerSecond := Value;
end;

procedure TRESTHandler.SetUseServiceKeyOnly(const Value: Boolean);
begin
  FUseServiceKeyOnly := Value;
end;

procedure TRESTHandler.SetFullLog(const Value: Boolean);
begin
  FFullLog := Value;
end;

{ TResponse }

function TResponse.GetJSONValue: TJSONValue;
begin
  if not JSON.IsEmpty then
    Result := TJSONObject.ParseJSONValue(JSON)
  else
    Result := nil;
end;

function TResponse.GetValue<T>(var Value: T): Boolean;
begin
  Result := Success;
  if Result then
  try
    Value := TJson.JsonToObject<T>(JSON);
  except
    Result := False;
  end;
end;

function TResponse.GetValue<T>(const Field: string; var Value: T): Boolean;
var
  JSONItem: TJSONValue;
begin
  Result := Success;
  if Result then
  try
    JSONItem := GetJSONValue;
    try
      Value := TJson.JsonToObject<T>(TJSONObject(JSONItem.TryGetValue(Field, Value)));
    finally
      JSONItem.Free;
    end;
  except
    Result := False;
  end;
end;

function TResponse.GetValues<T>(var Value: T): Boolean;
var
  JSONItem: TJSONValue;
begin
  Result := Success;
  if Result then
  try
    JSONItem := TJSONObject.ParseJSONValue(AppendItemsTag(JSON));
    try
      Value := TJson.JsonToObject<T>(TJSONObject(JSONItem));
    finally
      JSONItem.Free;
    end;
  except
    Result := False;
  end;
end;

function TResponse.AppendItemsTag(JSON: string): string;
begin
  Result := '{"Items": ' + JSON + '}';
end;

{ TArrayOfIntegerHelper }

function TArrayOfIntegerHelper.Add(Value: Integer): Integer;
begin
  Result := System.Length(Self) + 1;
  SetLength(Self, Result);
  Self[Result - 1] := Value;
end;

procedure TArrayOfIntegerHelper.Delete(const Value: Integer);
var
  i: Integer;
begin
  i := IndexOf(Value);
  if i >= 0 then
    System.Delete(Self, i, 1);
end;

function TArrayOfIntegerHelper.IndexOf(const Value: Integer): Integer;
var
  i: Integer;
begin
  for i := Low(Self) to High(Self) do
    if Self[i] = Value then
      Exit(i);
  Result := -1;
end;

function TArrayOfIntegerHelper.IsEmpty: Boolean;
begin
  Result := System.Length(Self) = 0;
end;

function TArrayOfIntegerHelper.Length: Integer;
begin
  Result := System.Length(Self);
end;

function TArrayOfIntegerHelper.ToJson: string;
var
  i: Integer;
begin
  Result := '[';
  for i := Low(Self) to High(Self) do
  begin
    if i <> Low(Self) then
      Result := Result + ',';
    Result := Result + '"' + Self[i].ToString + '"';
  end;
  Result := Result + ']';
end;

function TArrayOfIntegerHelper.ToString: string;
var
  i: Integer;
begin
  for i := Low(Self) to High(Self) do
  begin
    if i <> Low(Self) then
      Result := Result + ',';
    Result := Result + Self[i].ToString;
  end;
end;

{ TArrayOfStringHelper }

function TArrayOfStringHelper.IndexOf(const Value: string): Integer;
var
  i: Integer;
begin
  for i := Low(Self) to High(Self) do
    if Self[i] = Value then
      Exit(i);
  Result := -1;
end;

function TArrayOfStringHelper.IsEmpty: Boolean;
begin
  Result := System.Length(Self) = 0;
end;

function TArrayOfStringHelper.Length: Integer;
begin
  Result := System.Length(Self);
end;

function TArrayOfStringHelper.ToJson: string;
var
  i: Integer;
begin
  Result := '[';
  for i := Low(Self) to High(Self) do
  begin
    if i <> Low(Self) then
      Result := Result + ',';
    Result := Result + '"' + Self[i] + '"';
  end;
  Result := Result + ']';
end;

function TArrayOfStringHelper.ToString: string;
var
  i: Integer;
begin
  for i := Low(Self) to High(Self) do
  begin
    if i <> Low(Self) then
      Result := Result + ',';
    Result := Result + Self[i];
  end;
end;

function TArrayOfStringHelper.Add(const Value: string): Integer;
begin
  Result := System.Length(Self) + 1;
  SetLength(Self, Result);
  Self[Result - 1] := Value;
end;

procedure TArrayOfStringHelper.Assign(Source: TStrings);
var
  i: Integer;
begin
  SetLength(Self, Source.Count);
  for i := 0 to Source.Count - 1 do
    Self[i] := Source[i];
end;

procedure TArrayOfStringHelper.Delete(const Value: string);
var
  i: Integer;
begin
  i := IndexOf(Value);
  if i >= 0 then
    System.Delete(Self, i, 1);
end;

{ TParamsHelper }

function TParamsHelper.AddParam(var Dest: TParams; Param: TParam): Integer;
var
  i: Integer;
begin
  for i := Low(Dest) to High(Dest) do
    if Dest[i][0] = Param[0] then
    begin
      Dest[i] := Param;
      Exit(i);
    end;
  Result := Length(Dest) + 1;
  SetLength(Dest, Result);
  Dest[Result - 1] := Param;
end;

function TParamsHelper.Add(Param: TParam): Integer;
begin
  Result := AddParam(Self, Param);
end;

function TParamsHelper.Add(Key, Value: string): Integer;
begin
  Result := AddParam(Self, [Key, Value]);
end;

function TParamsHelper.Add(Key: string; Value: Integer): Integer;
begin
  Result := AddParam(Self, [Key, Value.ToString]);
end;

function TParamsHelper.Add(Key: string; Value: Boolean): Integer;
begin
  Result := AddParam(Self, [Key, BoolToString(Value)]);
end;

function TParamsHelper.Add(Key: string; Value: TArrayOfInteger): Integer;
begin
  Result := AddParam(Self, [Key, Value.ToString]);
end;

function TParamsHelper.Add(Key: string; Value: TDateTime; Format: string): Integer;
begin
  if Format.IsEmpty then
    Result := AddParam(Self, [Key, DateTimeToUnix(Value, False).ToString])
  else
    Result := AddParam(Self, [Key, FormatDateTime(Format, Value)]);
end;

function TParamsHelper.Add(Key: string; Value: Extended): Integer;
begin
  Result := AddParam(Self, [Key, Value.ToString]);
end;

function TParamsHelper.GetValue(Key: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := Low(Self) to High(Self) do
    if Self[i][0] = Key then
      Exit(Self[i][1]);
end;

function TParamsHelper.Add(Key: string; Value: TArrayOfString): Integer;
begin
  Result := AddParam(Self, [Key, Value.ToString]);
end;

function TParamsHelper.KeyExists(Key: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(Self) to High(Self) do
    if Self[i][0] = Key then
      Exit(True);
end;

function TParamsHelper.Remove(Key: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := Low(Self) to High(Self) do
    if Self[i][0] = Key then
    begin
      Delete(Self, i, 1);
      Break;
    end;
end;

end.

