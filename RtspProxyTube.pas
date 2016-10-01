unit RtspProxyTube;

interface

uses
  Classes, Sockets, WinSock;

type
  TRtspProxyNodeThread = class;

  TRtspPorts = record
    Data1, Sync1: TSocketPort;
    Data2, Sync2: TSocketPort;
  end;

  TRtspProxyNode = class
  private
    FSocket: TIpSocket;
    FLocalAddress: TSocketHost;
    FLocalPort: TSocketPort;
    FRemoteAddress: TSocketHost;
    FRemotePort: TSocketPort;
    FRemoteSockAddr: TSockAddr;
    FPair: TRtspProxyNode;
    FLogPacket: Boolean;
    FThread: TRtspProxyNodeThread;
    FIsRtp: Boolean;
    procedure Send(var Buf; BufSize: Integer);
  public
    constructor Create(LocalAddress: TSocketHost; LocalPort: TSocketPort; RemoteAddress: TSocketHost);
    destructor Destroy; override;
    procedure Open;
    procedure Start;
    procedure Close;
    function ToString: String;
    property LogPacket: Boolean read FLogPacket write FLogPacket;
    property LocalPort: TSocketPort read FLocalPort;
    property RemotePort: TSocketPort read FRemotePort write FRemotePort;
  end;

  TRtspProxyNodeThread = class(TThread)
  private
    FNode: TRtspProxyNode;
  protected
    procedure Execute; override;
  end;

  TRtspProxyTube = class
  private
    FActive: Boolean;
    FPortsGroup: Integer;
    FProxyData1: TRtspProxyNode;
    FProxySync1: TRtspProxyNode;
    FProxyData2: TRtspProxyNode;
    FProxySync2: TRtspProxyNode;
    FServerData1: TRtspProxyNode;
    FServerSync1: TRtspProxyNode;
    FServerData2: TRtspProxyNode;
    FServerSync2: TRtspProxyNode;
  public
    constructor Create(
      ClientAddress: TSocketHost; LocalForClient: TSocketHost;
      ServerAddress: TSocketHost; LocalForServer: TSocketHost);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    property ProxyData1: TRtspProxyNode read FProxyData1;
    property ProxySync1: TRtspProxyNode read FProxySync1;
    property ProxyData2: TRtspProxyNode read FProxyData2;
    property ProxySync2: TRtspProxyNode read FProxySync2;
    property ServerData1: TRtspProxyNode read FServerData1;
    property ServerSync1: TRtspProxyNode read FServerSync1;
    property ServerData2: TRtspProxyNode read FServerData2;
    property ServerSync2: TRtspProxyNode read FServerSync2;
  end;

var
  MinClientPort: Integer = 30000;
  MinServerPort: Integer = 40000;
  MaxProxyCount: Integer = 10;
  LogRtpPackets: Boolean = False;
  LogRtcpPackets: Boolean = False;

implementation

uses
  SysUtils, StrUtils, Log, RtspTypes;

type TSocketAccess = class(TIpSocket);

function GetPortsGroup: Integer; forward;
function GetServerPorts(Group: Integer): TRtspPorts; forward;
function GetClientPorts(Group: Integer): TRtspPorts; forward;
procedure ReleasePortsGroup(Group: Integer); forward;

{$region 'TRtspProxyTube'}
constructor TRtspProxyTube.Create(
  ClientAddress: TSocketHost; LocalForClient: TSocketHost;
  ServerAddress: TSocketHost; LocalForServer: TSocketHost);
var
  ServerPorts: TRtspPorts;
  ClientPorts: TRtspPorts;
begin
  FPortsGroup := GetPortsGroup;
  if FPortsGroup < 0 then
    raise Exception.Create('Unable to allocate a new ports group');

  ServerPorts := GetServerPorts(FPortsGroup);
  ClientPorts := GetClientPorts(FPortsGroup);

  // Создаем узлы, смотрящие на клиента
  FProxyData1 := TRtspProxyNode.Create(LocalForClient, ClientPorts.Data1, clientAddress);
  FProxySync1 := TRtspProxyNode.Create(LocalForClient, ClientPorts.Sync1, clientAddress);
  FProxyData2 := TRtspProxyNode.Create(LocalForClient, ClientPorts.Data2, clientAddress);
  FProxySync2 := TRtspProxyNode.Create(LocalForClient, ClientPorts.Sync2, clientAddress);

  // Создаем узлы, смотрящие на сервер
  FServerData1 := TRtspProxyNode.Create(LocalForServer, ServerPorts.Data1, serverAddress);
  FServerSync1 := TRtspProxyNode.Create(LocalForServer, ServerPorts.Sync1, serverAddress);
  FServerData2 := TRtspProxyNode.Create(LocalForServer, ServerPorts.Data2, serverAddress);
  FServerSync2 := TRtspProxyNode.Create(LocalForServer, ServerPorts.Sync2, serverAddress);

  FProxyData1.FPair := FServerData1;
  FProxySync1.FPair := FServerSync1;
  FProxyData2.FPair := FServerData2;
  FProxySync2.FPair := FServerSync2;

  FServerData1.FPair := FProxyData1;
  FServerSync1.FPair := FProxySync1;
  FServerData2.FPair := FProxyData2;
  FServerSync2.FPair := FProxySync2;

  FProxyData1.FIsRtp := True;
  FProxyData2.FIsRtp := True;
  FServerData1.FIsRtp := True;
  FServerData2.FIsRtp := True;

  FProxyData1.FLogPacket := LogRtpPackets;
  FProxyData2.FLogPacket := LogRtpPackets;
  FServerData1.FLogPacket := LogRtpPackets;
  FServerData2.FLogPacket := LogRtpPackets;

  FProxySync1.FLogPacket := LogRtcpPackets;
  FProxySync2.FLogPacket := LogRtcpPackets;
  FServerSync1.FLogPacket := LogRtcpPackets;
  FServerSync2.FLogPacket := LogRtcpPackets;
end;

destructor TRtspProxyTube.Destroy;
begin
  FProxyData1.Free;
  FProxySync1.Free;
  FProxyData2.Free;
  FProxySync2.Free;

  FServerData1.Free;
  FServerSync1.Free;
  FServerData2.Free;
  FServerSync2.Free;

  ReleasePortsGroup(FPortsGroup);
end;

procedure TRtspProxyTube.Start;
begin
  if FActive then Exit;

  // FProxyData1 и FProxyData2 не надо слушать,
  // Клиент не передает данные, а только получает их.
  // Поэтому не вызываем у них метод Start.

  FProxyData1.Open;

  FProxySync1.Open;
  FProxySync1.Start;

  FProxyData2.Open;

  FProxySync2.Open;
  FProxySync2.Start;


  FServerData1.Open;
  FServerData1.Start;

  FServerSync1.Open;
  FServerSync1.Start;

  FServerData2.Open;
  FServerData2.Start;

  FServerSync2.Open;
  FServerSync2.Start;

  FActive := True;
end;

procedure TRtspProxyTube.Stop;
begin
  if FActive then
  begin
    FServerData1.Close;
    FServerSync1.Close;
    FServerData2.Close;
    FServerSync2.Close;

    FProxyData1.Close;
    FProxySync1.Close;
    FProxyData2.Close;
    FProxySync2.Close;

    FActive := False;
  end;
end;
{$endregion}

{$region 'TRtspProxyNode'}
constructor TRtspProxyNode.Create(LocalAddress: TSocketHost; LocalPort: TSocketPort; RemoteAddress: TSocketHost);
begin
  FRemoteAddress := RemoteAddress;
  FLocalAddress := LocalAddress;
  FLocalPort := LocalPort;
end;

destructor TRtspProxyNode.Destroy;
begin
  FSocket.Free;
  inherited;
end;

function TRtspProxyNode.ToString: String;
begin
  Result := Format('%s local{%s:%s} <-> remote{%s:%s}',
    [IfThen(FIsRtp, 'RTP', 'RTCP'), FLocalAddress, FLocalPort, FRemoteAddress, FRemotePort]);
end;

procedure TRtspProxyNode.Open;
var
  S: String;
begin
  FSocket := TIpSocket.Create(nil);
  FSocket.SockType := stDgram;
  FSocket.Protocol := IPPROTO_UDP;
  FSocket.LocalHost := FLocalAddress;
  FSocket.LocalPort := FLocalPort;
  FSocket.Open;

  if not FSocket.Active then
  begin
    S := Format('[%s][Open] WSA error: %d', [ToString, WSAGetLastError]);
    WriteError(S);
    raise Exception.Create(S);
  end;

  if not TSocketAccess(FSocket).Bind then
  begin
    S := Format('[%s][Bind] WSA error: %d', [ToString, WSAGetLastError]);
    WriteError(S);
    raise Exception.Create(S);
  end;

  FRemoteSockAddr := FSocket.GetSocketAddr(FRemoteAddress, FRemotePort);

  WriteLine('[%s] Opened', [ToString]);
end;

procedure TRtspProxyNode.Close;
begin
  if Assigned(FSocket) then FSocket.Close;
end;

procedure TRtspProxyNode.Start;
begin
  WriteLine('[%s] Starting...', [ToString]);
  FThread := TRtspProxyNodeThread.Create(True);
  FThread.Priority := tpHighest;
  FThread.FreeOnTerminate := True;
  FThread.FNode := Self;
  FThread.Resume;
end;

procedure TRtspProxyNode.Send(var Buf; BufSize: Integer);
var
  ErrCode: Integer;
begin
  if Assigned(FSocket) then
    if FSocket.SendTo(Buf, BufSize, FRemoteSockAddr) < 0 then
    begin
      ErrCode := WSAGetLastError;
      case ErrCode of
        WSAENOTSOCK:
          WriteLine('[%s][Send] Canceled', [ToString]);
        else
          WriteError('[%s][Send] WSA error: %d', [ToString, ErrCode]);
      end;
    end;
end;

procedure TRtspProxyNodeThread.Execute;
var
  Buf: array[0..4095] of Byte;
  Count: Integer;
  ErrCode: Integer;
  NodeStr: String;
begin
  NodeStr := FNode.ToString;
  WriteLine('[%s] Started', [NodeStr]);

  while True do
  try
    // Count := FNode.FSocket.ReceiveBuf(Buf, 4096);
    // Пробуем оптимизировать - оставляем только вызов системной функции
    Count := recv(FNode.FSocket.Handle, Buf, 4069, 0);
    if Count <= 0 then
      raise ESocketError.Create;

    if FNode.FLogPacket then
      WriteLine('[%s] Bytes received: %d', [NodeStr, Count]);

    if Assigned(FNode.FPair)  then
    begin
      // FNode.FPair.Send(Buf, Count);
      // Пробуем оптимизировать - оставляем только вызов системной функции
      if Assigned(FNode.FPair.FSocket) then
      begin
        Count := WinSock.sendto(FNode.FPair.FSocket.Handle, Buf, Count, 0,
          FNode.FPair.FRemoteSockAddr, sizeof(FNode.FPair.FRemoteSockAddr));
        if Count < 0 then
        begin
          ErrCode := WSAGetLastError;
          case ErrCode of
            WSAENOTSOCK:
              WriteLine('[%s][Send] Canceled', [ToString]);
            else
              WriteError('[%s][Send] WSA error: %d', [ToString, ErrCode]);
          end;
        end;
      end;
    end;

  except
    on e: ESocketError do
    begin
      case e.ErrorCode of
        WSAECONNRESET:
          WriteLine('[%s] Remote is offline', [NodeStr]);
        WSAEINTR, WSAENOTSOCK, 0:
          WriteLine('[%s] Canceled', [NodeStr]);
        else
          WriteError('[%s][Receive] WSA error: %d', [NodeStr, e.ErrorCode]);
      end;
      Break;
    end;

    on e: Exception do
    begin
      WriteError('[%s][Receive] %s', [NodeStr, e.Message]);
      // По умолчанию ошибка заканчивает цикл общения с сервером.
      // Если ошибка не критичная и общение можно продолжить,
      // то нужно обработать ее отдельно.
      Break;
    end;
  end;

  FNode.FSocket.Close;

  WriteLine('[%s] Stopped', [NodeStr]);
end;
{$endregion}

{$region 'Port Groups'}
var
  BusyPortGroups: set of Byte;

function GetPortsGroup: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to MaxProxyCount-1 do
    if not (I in BusyPortGroups) then
    begin
      Result := I;
      Include(BusyPortGroups, I);
      Exit;
    end;
end;

procedure InitPortsGroup(var Ports: TRtspPorts; Offset, Group: Integer);
begin
  Ports.Data1 := IntToStr(Offset + Group * 4);
  Ports.Sync1 := IntToStr(Offset + Group * 4 + 1);
  Ports.Data2 := IntToStr(Offset + Group * 4 + 2);
  Ports.Sync2 := IntToStr(Offset + Group * 4 + 3);
end;

function GetServerPorts(Group: Integer): TRtspPorts;
begin
  InitPortsGroup(Result, MinServerPort, Group);
end;

function GetClientPorts(Group: Integer): TRtspPorts;
begin
  InitPortsGroup(Result, MinClientPort, Group);
end;

procedure ReleasePortsGroup(Group: Integer);
begin
  Exclude(BusyPortGroups, Group);
end;
{$endregion}

end.
