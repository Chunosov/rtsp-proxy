unit RtspProxyServer;

interface

uses
  SysUtils, Classes, Windows, Sockets, RtspTypes, RtspProxyTube;

type
  TProxyClient = class;
  TServerThread = class;

  TProxyServer = class
  private
    FProxy: TTcpServer;
    FClients: TList;
    FClientsLocker: TRTLCriticalSection;
    procedure AddClient(AClient: TProxyClient);
    procedure RemoveClient(AClient: TProxyClient);
    procedure ClientAccepted(Sender: TObject; Client: TCustomIpClient);
  public
    constructor Create(LocalPort: TSocketPort);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  end;

  TServerResponceEvent = procedure (Sender: TObject; Msg: TRtspMessage) of object;

  // Класс обслуживает одно клиентское подключение
  TProxyClient = class
  private
    FServer: TCustomIpClient;
    FClient: TCustomIpClient;
    FServerResource: String;
    FServerHost: TSocketHost;
    FServerPort: TSocketPort;
    FLocalResource: String;
    FLocalHost: TSocketHost;
    FLocalPort: TSocketPort;
    FProxyTube: TRtspProxyTube;
    FServerThread: TServerThread;
    procedure ConnectToServer(const Resource: String);
    procedure ProcessSetup1Responce(Sender: TObject; Msg: TRtspMessage);
    procedure ProcessSetup2Responce(Sender: TObject; Msg: TRtspMessage);
    procedure ProcessPlayResponce(Sender: TObject; Msg: TRtspMessage);
  public
    constructor Create(AClient: TCustomIpClient);
    destructor Destroy; override;
    procedure Listen;
    procedure Stop;
  end;

  TServerThread = class(TThread)
  private
    FOwner: TProxyClient;
    FClient: TCustomIpClient;
    FServer: TCustomIpClient;
    FOnResponce: TServerResponceEvent;
  public
    procedure Execute; override;
  end;

procedure LockProxy;
procedure UnlockProxy;

const
  BAD_ACTION_DISCARD = 1;
  BAD_ACTION_IGNORE = 2;

var
  BadClientRequestAction: Integer = BAD_ACTION_DISCARD;
  BadServerResponceAction: Integer = BAD_ACTION_DISCARD;
  LocalHostForClient: TSocketHost; // Локальный адрес, смотрящий на клиента
  LocalHostForServer: TSocketHost; // Локальный адрес, смотрящий на сервер

implementation

uses
  StrUtils, WinSock, Log;

var
  ProxyLocker: TRTLCriticalSection;

procedure LockProxy;
begin
  EnterCriticalSection(ProxyLocker);
end;

procedure UnlockProxy;
begin
  LeaveCriticalSection(ProxyLocker);
end;

{$region 'TProxyClient'}
constructor TProxyClient.Create(AClient: TCustomIpClient);
begin
  FClient := AClient;

  // Класс сокета на самом деле не умеет определять адрес сетевого интерфейса,
  // на который забинден сокет. Функция LocalHostAddr хоть и является методом,
  // на самом деле никакой информации о сокете, кажется, не использует.
  // В конечном итоге все сводится к вызову WSA функции gethostname().
  // Поэтому используем локальный адрес, взятый из конфига - LocalHostForClient.
  FLocalHost := LocalHostForClient;
  FLocalPort := FClient.LocalPort;
end;

destructor TProxyClient.Destroy;
begin
  FServer.Free;
  FServerThread.Free;
  FProxyTube.Free;
  inherited;
end;

procedure TProxyClient.Stop;
begin
  FServer.Close;
  // Все остальное закрывается автоматически по цепочке
end;

procedure TProxyClient.ConnectToServer(const Resource: String);
var
  Res: TRtspResource;
begin
  Resources.Lock;
  try
    Res := Resources.GetResource(Resource);
    if not Assigned(Res) then
      raise Exception.Create('Unknown resource: ' + Resource);

    FServerHost := Res.Host;
    FServerPort := Res.Port;
    FServerResource := Res.Name;
    FLocalResource := Resource;
  finally
    Resources.Unlock;
  end;

  // Создаем сокет, смотрящий на сервер
  //
  FServer := TTcpClient.Create(nil);
  FServer.LocalHost := LocalHostForServer; // номер порта пусть выбирается автоматически
  FServer.RemoteHost := FServerHost;
  FServer.RemotePort := FServerPort;
  FServer.Open;

  if not FServer.Connected then
    raise Exception.CreateFmt('Could not connect to server %s:%s. WSA error: %d',
      [FServerHost, FServerPort, WSAGetLastError]);

  FProxyTube := TRtspProxyTube.Create(
    FClient.RemoteHost, LocalHostForClient,
    FServer.RemoteHost, LocalHostForServer);

  FServerThread := TServerThread.Create(True);
  FServerThread.FOwner := Self;
  FServerThread.FClient := FClient;
  FServerThread.FServer := FServer;
  FServerThread.Resume;
end;

procedure TProxyClient.Listen;
var
  Buf: array[0..4095] of Char;
  Count: Integer;
  ClientStr, Request: String;
  Msg: TRtspMessage;
  Setup1Done: Boolean;
  Transport: TRtspTransportHeader;
begin
  ClientStr := FClient.RemoteHost + ':' + FClient.RemotePort;
  WriteLine('[%s] Client session opened', [ClientStr]);

  Setup1Done := False;
  while True do
  try
    Count := FClient.ReceiveBuf(buf, 4096);
    if Count <= 0 then
      raise ESocketError.Create;

    SetString(Request, buf, count);
    WriteLine('[%s] Client request (%d bytes):%s%s',
      [ClientStr, Count, CRLF, Request], ClientColor);

    try
      Msg := TRtspMessage.Create(Request);
    except
      on e: Exception do
        case BadClientRequestAction of
          BAD_ACTION_DISCARD:
          begin
            WriteError('[%s] %s DISCARDED', [ClientStr, e.Message]);
            Continue;
          end;

          BAD_ACTION_IGNORE:
          begin
            WriteError('[%s] %s IGNORED', [ClientStr, e.Message]);
            FServer.SendLn(Request, '');
            Continue;
          end

          else raise;
        end;
    end;

    if not Assigned(FServer) then
    begin
      ConnectToServer(Msg.Resource);
      WriteLine('[%s] Server connected', [ClientStr]);
    end;

    if Msg.Method = 'SETUP' then
    begin
      Transport := Msg.GetTransportHeader;
      if not Assigned(Transport) then
        raise Exception.Create('Field "Transport" is not specified in client RTSP-request');

      if not Setup1Done then
      begin
        Setup1Done := True;

        // Порты, которые прислал клиент, будут удаленными портами клиентской стороны "трубы"
        FProxyTube.ProxyData1.RemotePort := Transport.ClientPortData;
        FProxyTube.ProxySync1.RemotePort := Transport.ClientPortSync;

        // А к серверу коннектимся локальными портами серверной стороны "трубы"
        Transport.ClientPortData := FProxyTube.ServerData1.LocalPort;
        Transport.ClientPortSync := FProxyTube.ServerSync1.LocalPort;

        FServerThread.FOnResponce := ProcessSetup1Responce;
      end
      else
      begin
        // Порты, которые прислал клиент, будут удаленными портами клиентской стороны "трубы"
        FProxyTube.ProxyData2.RemotePort := Transport.ClientPortData;
        FProxyTube.ProxySync2.RemotePort := Transport.ClientPortSync;

        // А к серверу коннектимся локальными портами серверной стороны "трубы"
        Transport.ClientPortData := FProxyTube.ServerData2.LocalPort;
        Transport.ClientPortSync := FProxyTube.ServerSync2.LocalPort;

        FServerThread.FOnResponce := ProcessSetup2Responce;
      end;
    end
    else if Msg.Method = 'PLAY' then
    begin
      FServerThread.FOnResponce := ProcessPlayResponce;
    end
    else if Msg.Method = 'TEARDOWN' then
    begin
      if Assigned(FProxyTube) then FProxyTube.Stop;
    end;

    // Подменяем адрес прокси (т.е. локальный адрес и порт) на адрес реального сервера
    Msg.Address := FServerHost;
    Msg.Port := FServerPort;
    // Замена, а не простое присвоение, нужна т.к. во время SETUP
    // имя ресурса расширено. Например: rtsp://192.168.2.25/Channel2/track1
    Msg.Resource := AnsiReplaceText(Msg.Resource, FLocalResource, FServerResource);
    // Согласно стандарту, ссылка на ресурс всегда дается в абсолютной форме
    Msg.ReplaceAll(
      Format('rtsp://%s:%s/', [FLocalHost, FLocalPort]),
      Format('rtsp://%s:%s/', [FServerHost, FServerPort]));
    // Иногда адрес может не содержать порт, если он дефолтный. Например,
    // VLC возвращает "Content-Base=rtsp://192.168.197.128:554/Channel1"
    // а АВК (Live555) "Content-Base=rtsp://192.168.5.4/Channel1"
    Msg.ReplaceAll(
        Format('rtsp://{0}/', [FLocalHost]),
        Format('rtsp://{0}/', [FServerHost]));
    Msg.ReplaceAll(FLocalResource, FServerResource);

    Request := Msg.HeaderToString;

    WriteLine('[%s] Transformed client request (%d bytes):%s%s',
      [ClientStr, Length(Request), CRLF, Request], ClientProxyColor);

    FServer.SendLn(Request, ''); // TODO: check state(?)

    Msg.Free;
  except
    on e: ESocketError do
    begin
      case e.ErrorCode of
        WSAECONNRESET:
          WriteLine('[%s] Client is offline', [ClientStr]);
        WSAEINTR, WSAENOTSOCK, 0:
          WriteLine('[%s] Canceled', [ClientStr]);
        else
          WriteError('[%s][Receive] WSA error: %d', [ClientStr, e.ErrorCode]);
      end;
      Break;
    end;

    on e: Exception do
    begin
      if e.Message <> '' then
        WriteError('[%s][Receive] %s', [ClientStr, e.Message]);
      // По умолчанию ошибка заканчивает цикл общения с сервером.
      // Если ошибка не критичная и общение можно продолжить,
      // то нужно обработать ее отдельно.
      Break;
    end;
  end;

  WriteLine('[%s] Client session closed', [ClientStr]);

  if Assigned(FProxyTube) then
    FProxyTube.Stop;

  FClient.Close;

  if Assigned(FServer) then
    FServer.Close;
  if Assigned(FServerThread) then
    FServerThread.WaitFor;
end;

procedure TServerThread.Execute;
var
  Buf: array[0..4095] of Char;
  Count: Integer;
  ServerStr, Responce: String;
  Msg: TRtspMessage;
begin
  ServerStr := 'Server -> ' + FClient.RemoteHost + ':' + FClient.RemotePort;
  WriteLine('[%s] Server session opened', [ServerStr]);

  while True do
  try
    Count := FServer.ReceiveBuf(Buf, 4096);
    if Count <= 0 then
      raise ESocketError.Create;

    SetString(Responce, Buf, Count);
    WriteLine('[%s] Server responce (%d bytes):%s%s',
      [ServerStr, Count, CRLF, Responce], ServerColor);

    try
      Msg := TRtspMessage.Create(Responce);
    except
      on e: Exception do
        case BadServerResponceAction of
          BAD_ACTION_DISCARD:
          begin
            WriteError('[%s] %s DISCARDED', [ServerStr, e.Message]);
            Continue;
          end;

          BAD_ACTION_IGNORE:
          begin
            WriteError('[%s] %s IGNORED', [ServerStr, e.Message]);
            FClient.SendLn(Responce, '');
            Continue;
          end

          else raise;
        end;
    end;

    // VLC сначала присылает заголовок, потом тело сообщения отдельным пакетом
    while Msg.ContentLength > Length(Msg.Content) do
    begin
      WriteLine('[%s] Bytes received: %d; expecting more: %d',
        [ServerStr, Count, Msg.ContentLength - Length(Msg.Content)], ServerColor);

      Count := FServer.ReceiveBuf(Buf, 4096);
      if Count <= 0 then
        raise ESocketError.Create;

      SetString(Responce, Buf, Count);
      WriteLine('[%s] Server responce (%d bytes):%s%s',
        [ServerStr, Count, CRLF, Responce], ServerColor);

      Msg.Content := Msg.Content + Responce;
    end;

    if Assigned(FOnResponce) then
    begin
      FOnResponce(Self, Msg);
      FOnResponce := nil;
    end;

    // Подменяем адрес сервера на адрес прокси (т.е. локальный адрес и порт)
    // Согласно стандарту, ссылка на ресурс всегда дается в абсолютной форме
    Msg.ReplaceAll(
      Format('rtsp://%s:%s/', [FOwner.FServerHost, FOwner.FServerPort]),
      Format('rtsp://%s:%s/', [FOwner.FLocalHost, FOwner.FLocalPort]));
    // Иногда адрес может не содержать порт, если он дефолтный. Например,
    // VLC возвращает "Content-Base=rtsp://192.168.197.128:554/Channel1"
    // а АВК "Content-Base=rtsp://192.168.5.4/Channel1"
    Msg.ReplaceAll(
      Format('rtsp://%s/', [FOwner.FServerHost]),
      Format('rtsp://%s/', [FOwner.FLocalHost]));
    Msg.ReplaceAll(FOwner.FServerResource, FOwner.FLocalResource);

    Responce := Msg.HeaderToString;

    WriteLine('[%s] Transformed server responce (%d bytes):%s%s',
      [ServerStr, Length(Responce), CRLF, Responce], ServerProxyColor);

    FClient.SendLn(Responce, '');

    if Msg.Content <> '' then
    begin
      Responce := Msg.Content;

      WriteLine('[%s] Transformed server responce (%d bytes):%s%s',
        [ServerStr, Length(Responce), CRLF, Responce], ServerProxyColor);

      FClient.SendLn(Responce, ''); // TODO: check state(?)
    end;

    Msg.Free;
  except
    on e: ESocketError do
    begin
      case e.ErrorCode of
        WSAECONNRESET:
          WriteLine('[%s] Server is offline', [ServerStr]);
        WSAEINTR, WSAENOTSOCK, 0:
          WriteLine('[%s] Canceled', [ServerStr]);
        else
          WriteError('[%s][Receive] WSA error: %d', [ServerStr, e.ErrorCode]);
      end;
      Break;
    end;

    on e: Exception do
    begin
      if e.Message <> '' then
        WriteError('[%s][Receive]: %s', [ServerStr, e.Message]);
      // По умолчанию ошибка заканчивает цикл общения с сервером.
      // Если ошибка не критичная и общение можно продолжить,
      // то нужно обработать ее отдельно.
      Break;
    end;
  end;

  WriteLine('[%s] Server session closed', [ServerStr]);

  if Assigned(FOwner.FProxyTube) then
    FOwner.FProxyTube.Stop;

  FServer.Close;
  FClient.Close;
end;

procedure TProxyClient.ProcessSetup1Responce(Sender: TObject; Msg: TRtspMessage);
var
  Transport: TRtspTransportHeader;
begin
	Transport := Msg.GetTransportHeader;
	if not Assigned(Transport) then
    raise Exception.Create('Server did not specify field "Transport" in responce to the SETUP request');

  FProxyTube.ServerData1.RemotePort := Transport.ServerPortData;
  FProxyTube.ServerSync1.RemotePort := Transport.ServerPortSync;

  Transport.ServerPortData := FProxyTube.ProxyData1.LocalPort;
  Transport.ServerPortSync := FProxyTube.ProxySync1.LocalPort;
  Transport.ClientPortData := FProxyTube.ProxyData1.RemotePort;
  Transport.ClientPortSync := FProxyTube.ProxySync1.RemotePort;
end;

procedure TProxyClient.ProcessSetup2Responce(Sender: TObject; Msg: TRtspMessage);
var
  Transport: TRtspTransportHeader;
begin
	Transport := Msg.GetTransportHeader;
	if not Assigned(Transport) then
    raise Exception.Create('Server did not specify field "Transport" in responce to the SETUP request');

  FProxyTube.ServerData2.RemotePort := Transport.ServerPortData;
  FProxyTube.ServerSync2.RemotePort := Transport.ServerPortSync;

  Transport.ServerPortData := FProxyTube.ProxyData2.LocalPort;
  Transport.ServerPortSync := FProxyTube.ProxySync2.LocalPort;
  Transport.ClientPortData := FProxyTube.ProxyData2.RemotePort;
  Transport.ClientPortSync := FProxyTube.ProxySync2.RemotePort;
end;

procedure TProxyClient.ProcessPlayResponce(Sender: TObject; Msg: TRtspMessage);
begin
  FProxyTube.Start;
end;
{$endregion}

{$region 'TProxyServer'}
constructor TProxyServer.Create(LocalPort: TSocketPort);
begin
  InitializeCriticalSection(FClientsLocker);

  FClients := TList.Create;

  FProxy := TTcpServer.Create(nil);
  FProxy.LocalHost := LocalHostForClient;
  FProxy.LocalPort := LocalPort;
  FProxy.OnAccept := ClientAccepted;
end;

destructor TProxyServer.Destroy;
begin
  Stop;

  FClients.Free;
  FProxy.Free;

  DeleteCriticalSection(FClientsLocker);
  inherited;
end;

procedure TProxyServer.Start;
begin
  if not FProxy.Active then
  begin
    FProxy.Open;

    WriteLine('Proxy started at %s:%s', [FProxy.LocalHost, FProxy.LocalPort]);
  end;
end;

procedure TProxyServer.Stop;
begin
  if FProxy.Active then
  begin
    while FClients.Count > 0 do
      TProxyClient(FClients[0]).Stop;
    FProxy.Close;

    WriteLine('Proxy stopped at %s:%s', [FProxy.LocalHost, FProxy.LocalPort]);
  end;
end;

// Метод выполняется в отдельном потоке, автоматически создаваемом TCP-сервером.
// После завершения метода сервер закроет клиентской сокет, уничтожит объект AClient,
// а поток заснет и будет повторно использован при подключении другого клиента.
// TODO: Число потоков в пуле TCP-сервера ограничено (10), нужно как-то разрулить
// ситуацию, если вдруг одновременно захотят подключиться больше клиентов.
procedure TProxyServer.ClientAccepted(Sender: TObject; Client: TCustomIpClient);
var
  ClientStr: String;
  ProxyClient: TProxyClient;
begin
  ClientStr := Client.RemoteHost + ':' + Client.RemotePort;
  try
    WriteLine('Client Accepted: ' + ClientStr);

    ProxyClient := TProxyClient.Create(Client);
    try
      AddClient(ProxyClient);
      ProxyClient.Listen;
      RemoveClient(ProxyClient);
    finally
      ProxyClient.Free;
    end;
  except
    on e: Exception do
      WriteError('[%s][Accept] %s', [ClientStr, e.Message]);
  end;
end;

procedure TProxyServer.AddClient(AClient: TProxyClient);
begin
  EnterCriticalSection(FClientsLocker);
  try
    FClients.Add(AClient);
  finally
    LeaveCriticalSection(FClientsLocker);
  end;
end;

procedure TProxyServer.RemoveClient(AClient: TProxyClient);
begin
  EnterCriticalSection(FClientsLocker);
  try
    FClients.Remove(AClient);
  finally
    LeaveCriticalSection(FClientsLocker);
  end;
end;
{$endregion}

initialization
  InitializeCriticalSection(ProxyLocker);

finalization
  DeleteCriticalSection(ProxyLocker);

end.
