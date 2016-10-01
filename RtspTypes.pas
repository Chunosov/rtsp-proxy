unit RtspTypes;

interface

uses
  Classes, SysUtils, Windows, Sockets, WinSock;

type
  TRtspHeader = class;
  TRtspHeaders = array of TRtspHeader;
  TRtspTransportHeader = class;

  TRtspResource = class
  public
    Name: String;
    Host: TSocketHost;
    Port: TSocketPort;
    constructor Create(const RtspAddress: String);
    procedure Parse(const RtspAddress: String);
    function ToString: String;
  end;

  TRtspResourceCatalog = class
  private
    FLocker: TRTLCriticalSection;
    FResources: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure PrintAll;
    procedure Lock;
    procedure Unlock;
    procedure AppendResource(const Name, RtspAddress: String);
    procedure RemoveResource(const Name: String);
    function GetResource(const Name: String): TRtspResource;
  end;

  TRtspMessage = class
  private
    FAddress: TSocketHost;
    FPort: TSocketPort;
    FResource: String;
    FContent: String;
    FMethod: String;
    FIsResponce: Boolean;
    FErrorCode: String;
    FReasonPhrase: String;
    FCseq: String;
    FContentLength: Integer;
    FHeaders: TRtspHeaders;
    procedure ParsePacket(const APacket: String);
    procedure ParseFirstLine(const Line: String);
    procedure AddHeader(const AHeader: String; const AValue: String); overload;
    procedure AddHeader(AHeader: TRtspHeader); overload;
    function ExtractMessage(const APacket: String): String;
  public
    constructor Create(const APacket: String);
    destructor Destroy; override;
    function GetTransportHeader: TRtspTransportHeader;
    function HeaderToString: String;
    procedure ReplaceAll(const Source: String; const Dest: String);
    property Method: String read FMethod;
    property Address: TSocketHost read FAddress write FAddress;
    property Port: TSocketPort read FPort write FPort;
    property Resource: String read FResource write FResource;
    property ContentLength: Integer read FContentLength;
    property Content: String read FContent write FContent;
  end;

  TRtspHeader = class
  public
    Header: String;
    Value: String;
    function ToString: String; virtual;
  end;

  TRtspTransportHeader = class(TRtspHeader)
  private
    procedure ParsePorts(const S: String; out Port1: String; out Port2: String);
  public
    ClientPortData: String;
    ClientPortSync: String;
    ServerPortData: String;
    ServerPortSync: String;
    Values: array of string;

    constructor Create(const AValue: String);
    function ToString: String; override;
  end;

  ESocketError = class(Exception)
  public
    ErrorCode: Integer;
    constructor Create; reintroduce; overload;
    constructor Create(const Msg: String); overload;
  end;

const
  DefaultPort = '554';
  RtspPrefix = 'rtsp://';
	RtspVersion = 'RTSP/1.0';
	HeaderCseq = 'CSeq';
	HeaderContentLen = 'Content-Length';
	HeaderUserAgent = 'User-Agent';
	HeaderTransport = 'Transport';

var
  Resources: TRtspResourceCatalog;

implementation

uses
  StrUtils, Log;


{$region 'ESocketError'}
constructor ESocketError.Create;
begin
  ErrorCode := WSAGetLastError;
  inherited Create('');
end;

constructor ESocketError.Create(const Msg: String);
begin
  ErrorCode := WSAGetLastError;
  inherited;
end;
{$endregion}

{$region 'TRtspResource'}
constructor TRtspResource.Create(const RtspAddress: String);
begin
  Parse(RtspAddress);
end;

procedure TRtspResource.Parse(const RtspAddress: String);
var
  Index: Integer;
  Address: String;
begin
  if not AnsiStartsText(RtspPrefix, RtspAddress) then
    raise Exception.Create('Bad RTSP address: ' + RtspAddress);
  Address := Copy(RtspAddress, 8, MaxInt);
  Index := Pos(':', Address);
  if Index > 0 then
  begin
    Host := Copy(Address, 1, Index-1);
    Address := Copy(Address, Index+1, MaxInt);
    Index := Pos('/', Address);
    if Index > 0 then
    begin
      Port := Copy(Address, 1, Index-1);
      Name := Copy(Address, Index+1, MaxInt);
    end;
  end
  else
  begin
    Port := DefaultPort;
    Index := Pos('/', Address);
    if Index > 0 then
    begin
      Host := Copy(Address, 0, Index-1);
      Name := Copy(Address, Index+1, MaxInt);
    end;
  end;
  Host := Trim(Host);
  Name := Trim(Name);
  // Мы не можем работать с адресом * или с адресом без имени ресурса
  // (например, встречается такое "OPTIONS 192.168.5.4:554 RTSP/1.0")
  // т.к. в этом случае невозможно определить на какой сервер
  // перенаправить запрос. Ведь в общем случае их может быть несколько.
  if (Host = '') or (Name = '') then
    raise Exception.Create('Bad RTSP address: ' + RtspAddress);
end;

function TRtspResource.ToString: String;
begin
  Result := Format('%s%s:%s/%s', [RtspPrefix, Host, Port, Name]);
end;
{$endregion}

{$region 'TRtspMessage'}
constructor TRtspMessage.Create(const APacket: String);
begin
  FPort := DefaultPort;

  ParsePacket(APacket);
end;

destructor TRtspMessage.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FHeaders)-1 do
    FHeaders[I].Free;
  inherited;
end;

procedure TRtspMessage.ParsePacket(const APacket: String);
var
  Lines: TStrings;
  Packet: String;
  Index: Integer;
  Line, Header, Value: String;
begin
  Packet := ExtractMessage(APacket);

  Lines := TStringList.Create;
  try
    Lines.Text := Packet;

    if Lines.Count > 0 then
    begin
      ParseFirstLine(Lines[0]);
      Lines.Delete(0);
    end;

    for Line in Lines do
    begin
      Index := Pos(':', Line);
      if Index = 0 then
        raise Exception.Create('Invalid line in RTSP message header: ' + Line);

      Header := Trim(Copy(Line, 1, Index-1));
      Value := Trim(Copy(Line, Index+1, MaxInt));

      if AnsiSameText(Header, HeaderCseq) then
        FCseq := Value
      else if AnsiSameText(Header, HeaderContentLen) then
        FContentLength := StrToInt(Value)
      else if AnsiSameText(Header, HeaderTransport) then
        AddHeader(TRtspTransportHeader.Create(Value))
      else
        AddHeader(Header, Value);
    end;

		if Length(FContent) > FContentLength then
    	raise Exception.CreateFmt('Invalid content length of RTSP message. ' +
    		'Expected: %d bytes, got: %d bytes', [FContentLength, Length(FContent)]);
  finally
    Lines.Free;
  end;
end;

function TRtspMessage.ExtractMessage(const APacket: String): String;
var
  Index: Integer;
begin
  Index := Pos(#$D#$A#$D#$A, APacket);
  if Index > 0 then
  begin
    FContent := Copy(APacket, Index+4, MaxInt);
    Result := Copy(APacket, 1, Index-1);
  end
  else
    Result := APacket;
end;

procedure TRtspMessage.ParseFirstLine(const Line: String);
var
  Index: Integer;
  Source: String;
  Res: TRtspResource;
begin
  Source := Line;

  Index := Pos(' ', Source);
  if Index = 0 then
    raise Exception.Create('Invalid request or status line: ' + Line);

  FMethod := Copy(Source, 1, Index-1);
  Source := Copy(Source, Index+1, MaxInt);

  Index := Pos(' ', Source);
  if Index = 0 then
    raise Exception.Create('Invalid request or status line: ' + Line);

  if FMethod = RtspVersion then
  begin // status line
    FIsResponce := True;
    FErrorCode := Copy(Source, 1, Index-1);
    FReasonPhrase := Copy(Source, Index+1, MaxInt);
  end
  else
  begin // request line
    Res := TRtspResource.Create(Copy(Source, 1, Index-1));
    FAddress := Res.Host;
    FPort := Res.Port;
    FResource := Res.Name;
    Res.Free;
  end;
end;

procedure TRtspMessage.AddHeader(const AHeader: String; const AValue: String);
var
  Header: TRtspHeader;
begin
  Header := TRtspHeader.Create;
  Header.Header := AHeader;
  Header.Value := AValue;
  AddHeader(Header);
end;

procedure TRtspMessage.AddHeader(AHeader: TRtspHeader);
var
  L: Integer;
begin
  L := Length(FHeaders);
  SetLength(FHeaders, L+1);
  FHeaders[L] := AHeader;
end;

function TRtspMessage.GetTransportHeader: TRtspTransportHeader;
var
  Header: TRtspHeader;
begin
  for Header in FHeaders do
    if Header is TRtspTransportHeader then
    begin
      Result := Header as TRtspTransportHeader;
      Exit;
    end;
  Result := nil;
end;

procedure TRtspMessage.ReplaceAll(const Source: String; const Dest: String);
var
  Header: TRtspHeader;
begin
  for Header in FHeaders do
    Header.Value := AnsiReplaceText(Header.Value, Source, Dest);

  if FContent <> '' then
    FContent := AnsiReplaceText(FContent, Source, Dest);
end;

function TRtspMessage.HeaderToString: String;
var
  Strs: TStrings;
  Header: TRtspHeader;
begin
  Strs := TStringList.Create;
  try
    if FIsResponce then
      Strs.Append(Format('%s %s %s', [RtspVersion, FErrorCode, FReasonPhrase]))
    else
    begin
      if (FAddress = '') or (FResource = '') then
        Strs.Append(Format('%s * %s', [FMethod,  RtspVersion]))
      else
        Strs.Append(Format('%s rtsp://%s:%s/%s %s',
          [FMethod, FAddress, FPort, FResource, RtspVersion]));
    end;
    Strs.Append(HeaderCseq + ': ' + FCseq);
    if FContent <> '' then
      Strs.Append(HeaderContentLen + ': ' + IntToStr(Length(FContent)));
    for Header in FHeaders do
      Strs.Append(Header.ToString);
    Strs.Append('');
    Result := Strs.Text;
  finally
    Strs.Free;
  end;
end;
{$endregion}

{$region 'TRtspHeader'}
function TRtspHeader.ToString: String;
begin
  Result := Header + ': ' + Value;
end;
{$endregion}

{$region 'TRtspTransportHeader'}
constructor TRtspTransportHeader.Create(const AValue: String);
var
  Strs: TStrings;
  S: String;
  L: Integer;
begin
  Header := HeaderTransport;
  Value := AValue;

  Strs := TStringList.Create;
  try
    Strs.Delimiter := ';';
    Strs.DelimitedText := AValue;

    for S in Strs do
    begin
      if AnsiStartsText('client_port', S) then
        ParsePorts(S, ClientPortData, ClientPortSync)
      else if AnsiStartsText('server_port', S) then
        ParsePorts(S, ServerPortData, ServerPortSync)
      else
      begin
        L := Length(Values);
        SetLength(Values, L+1);
        Values[L] := S;
      end;
    end;
  finally
    Strs.Free;
  end;
end;

procedure TRtspTransportHeader.ParsePorts(const S: String; out Port1: String; out Port2: String);
var
  Index: Integer;
  Ports: String;
begin
  Index := Pos('=', S);
  if Index > 0 then
  begin
    Ports := Copy(S, Index+1, MaxInt);
    Index := Pos('-', Ports);
    if Index > 0 then
    begin
      Port1 := Trim(Copy(Ports, 1, Index-1));
      Port2 := Trim(Copy(Ports, Index+1, MaxInt));
      Exit;
    end;
  end;
  raise Exception.Create('Unable to extract port numbers from Transport header');
end;

function TRtspTransportHeader.ToString: String;
var
  S: String;
begin
  Result := HeaderTransport + ': ';
  for S in Values do
  begin
    if AnsiStartsText('source=', S) then Continue;
    if AnsiStartsText('destination=', S) then Continue;
    Result := Result + S + ';';
  end;
  if (ClientPortData <> '') and (ClientPortSync <> '') then
    Result := Result + Format('client_port=%s-%s;', [ClientPortData, ClientPortSync]);
  if (ServerPortData <> '') and (ServerPortSync <> '') then
    Result := Result + Format('server_port=%s-%s;', [ServerPortData, ServerPortSync]);
  SetLength(Result, Length(Result)-1);
end;
{$endregion}

{$region 'TRtspResourceCatalog'}
constructor TRtspResourceCatalog.Create;
begin
  InitializeCriticalSection(FLocker);
  FResources := TStringList.Create;
end;

destructor TRtspResourceCatalog.Destroy;
var
  I: Integer;
begin
  for I := 0 to FResources.Count-1 do
    FResources.Objects[I].Free;
  FResources.Free;
  DeleteCriticalSection(FLocker);
  inherited;
end;

procedure TRtspResourceCatalog.Lock;
begin
  EnterCriticalSection(FLocker);
end;

procedure TRtspResourceCatalog.Unlock;
begin
  LeaveCriticalSection(FLocker);
end;

procedure TRtspResourceCatalog.AppendResource(const Name, RtspAddress: String);
var
  Index: Integer;
  Res: TRtspResource;
begin
  Res := TRtspResource.Create(RtspAddress);
  Lock;
  try
    Index := FResources.IndexOf(Name);
    if Index < 0 then
    begin
      FResources.AddObject(Name, Res);
      WriteLine('Resource added: %s <-> %s', [Name, Res.ToString]);
    end
    else
    begin
      FResources.Objects[Index].Free;
      FResources.Objects[Index] := Res;
      WriteLine('Resource updated: %s <-> %s', [Name, Res.ToString]);
    end;
  finally
    Unlock;
  end;
end;

procedure TRtspResourceCatalog.RemoveResource(const Name: String);
var
  Index: Integer;
  Res: TRtspResource;
begin
  Lock;
  try
    Index := FResources.IndexOf(Name);
    if Index >= 0 then
    begin
      Res := TRtspResource(FResources.Objects[Index]);
      FResources.Delete(Index);
      WriteLine('Resource removed: %s <-> %s', [Name, Res.ToString]);
      Res.Free;
    end;
  finally
    Unlock;
  end;
end;

function TRtspResourceCatalog.GetResource(const Name: String): TRtspResource;
var
  Index: Integer;
begin
  // GetResource должен блокироваться снаружи, там где он вызывается,
  // на все время использования полученного ресурса
  Index := FResources.IndexOf(Name);
  if Index >= 0
    then Result := TRtspResource(FResources.Objects[Index])
    else Result := nil;
end;

procedure TRtspResourceCatalog.PrintAll;
var
  I: Integer;
  Res: TRtspResource;
begin
  for I := 0 to FResources.Count-1 do
  begin
    Res := TRtspResource(FResources.Objects[I]);
    WriteLn(Format('%s -> %s:%s/%s', [FResources[I], Res.Host, Res.Port, Res.Name]));
  end;
  if FResources.Count = 0 then
    WriteLn('Resources were not added');
end;
{$endregion}

initialization
  Resources := TRtspResourceCatalog.Create;

finalization
  Resources.Free;

end.
