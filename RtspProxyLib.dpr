library RtspProxyLib;

uses
  Windows,
  Classes,
  SysUtils,
  IniFiles,
  Log in 'Log.pas',
  Console in 'Console.pas',
  RtspTypes in 'RtspTypes.pas',
  RtspProxyTube in 'RtspProxyTube.pas',
  RtspProxyServer in 'RtspProxyServer.pas';

{                         Console
                             |
             <------------- Log ------------->
             |               |               |
             |<--------- RtspTypes --------->|
             |                               |
       RtspProxyServer <------------- RtspProxyTube
}

var
  Proxy: TProxyServer = nil;
  ProxyPort: Integer = 554;

function GetLocalPath: String;
var
  FileName: array[0..MAX_PATH] of Char;
begin
  GetModuleFileName(hInstance, FileName, MAX_PATH);
  Result := ExtractFilePath(FileName);
end;

function GetIniFileName: String;
begin
  Result := GetLocalPath + 'RtspProxy.ini';
end;

procedure ReadSettings;
const
  Section = 'RtspProxy';
  DefLogFile = 'RtspProxy.log';
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(GetIniFileName);
  try
    ProxyPort := Ini.ReadInteger(Section, 'ProxyPort', 554);

    Log.LogToConsole := Ini.ReadBool(Section, 'LogToConsole', False);
    Log.LogToFile := Ini.ReadBool(Section, 'LogToFile', False);
    RtspProxyTube.LogRtpPackets := Ini.ReadBool(Section, 'LogRtpPackets', False);
    RtspProxyTube.LogRtcpPackets := Ini.ReadBool(Section, 'LogRtcpPackets', False);
    if Log.LogToFile then
    begin
      LogFileName := Trim(Ini.ReadString(Section, 'LogFileName', DefLogFile));
      if LogFileName = '' then LogFileName := DefLogFile;
      // ≈сли путь не задан или задан относительно (e.g.: .\Logs\LogFile.log)
      if (LogFileName[1] = '.') or (ExtractFilePath(LogFileName) = '') then
        LogFileName := GetLocalPath + LogFileName;
    end;

    RtspProxyTube.MinClientPort := Ini.ReadInteger(Section, 'MinClientPort', 30000);
    RtspProxyTube.MinServerPort := Ini.ReadInteger(Section, 'MinServerPort', 40000);
    RtspProxyTube.MaxProxyCount := Ini.ReadInteger(Section, 'MaxProxyCount', 10);

    RtspProxyServer.BadClientRequestAction := Ini.ReadInteger(Section, 'BadClientRequestAction', 0);
    RtspProxyServer.BadServerResponceAction := Ini.ReadInteger(Section, 'BadServerResponceAction', 0);
    RtspProxyServer.LocalHostForClient := Ini.ReadString(Section, 'LocalHostForClient', '0.0.0.0');
    RtspProxyServer.LocalHostForServer := Ini.ReadString(Section, 'LocalHostForServer', '0.0.0.0');
  finally
    Ini.Free;
  end;
end;

procedure PrintSettings;

  function BadActionToStr(Action: Integer): String;
  begin
    Result := IntToStr(Action);
    case Action of
      BAD_ACTION_DISCARD: Result := Result + ' (DISCARD)';
      BAD_ACTION_IGNORE: Result := Result + ' (IGNORE)';
      else Result := Result + ' (STOP)';
    end;
  end;

var
  Strs: TStrings;
begin
  Strs := TStringList.Create;
  try
    Strs.Add('Settings:');
    Strs.Add('SettingsFile = ' + GetIniFileName);
    Strs.Add('LogToConsole = ' + BoolToStr(Log.LogToConsole, True));
    Strs.Add('LogToFile = ' + BoolToStr(Log.LogToFile, True));
    Strs.Add('LogRtpPackets = ' + BoolToStr(RtspProxyTube.LogRtpPackets, True));
    Strs.Add('LogRtcpPackets = ' + BoolToStr(RtspProxyTube.LogRtcpPackets, True));
    Strs.Add('ProxyPort = ' + IntToStr(ProxyPort));
    Strs.Add('LogFileName = ' + Log.LogFileName);
    Strs.Add('MinClientPort = ' + IntToStr(RtspProxyTube.MinClientPort));
    Strs.Add('MinServerPort = ' + IntToStr(RtspProxyTube.MinServerPort));
    Strs.Add('MaxProxyCount = ' + IntToStr(RtspProxyTube.MaxProxyCount));
    Strs.Add('LocalHostForClient = ' + RtspProxyServer.LocalHostForClient);
    Strs.Add('LocalHostForServer = ' + RtspProxyServer.LocalHostForServer);
    Strs.Add('BadClientRequestAction = ' + BadActionToStr(RtspProxyServer.BadClientRequestAction));
    Strs.Add('BadServerResponceAction = ' + BadActionToStr(RtspProxyServer.BadServerResponceAction));
    WriteLine(Strs.Text);
  finally
    Strs.Free;
  end;
end;

{$region 'Exports'}
procedure InitProxy; cdecl;
begin
  try
    ReadSettings;
    PrintSettings;
  except
    on E: Exception do
      WriteError('[InitProxy] %s: %s', [E.ClassName, E.Message]);
  end;
end;

procedure StartProxyOnPort(LocalPort: Integer); cdecl;
begin
  try
    LockProxy;
    try
      if not Assigned(Proxy) then
      begin
        Proxy := TProxyServer.Create(IntToStr(LocalPort));
        Proxy.Start;
      end;
    finally
      UnlockProxy;
    end;
  except
    on E: Exception do
      WriteError('[StartProxyOnPort] %s: %s', [E.ClassName, E.Message]);
  end;
end;

procedure StartProxy; cdecl;
begin
  StartProxyOnPort(ProxyPort);
end;

procedure AppendResource(Name, RtspAddress: PChar); cdecl;
begin
  try
    Resources.AppendResource(Name, RtspAddress);
  except
    on E: Exception do
      WriteError('[AppendResource] %s: %s', [E.ClassName, E.Message]);
  end;
end;

procedure RemoveResource(Name: PChar); cdecl;
begin
  try
    Resources.RemoveResource(Name);
  except
    on E: Exception do
      WriteError('[RemoveResource] %s: %s', [E.ClassName, E.Message]);
  end;
end;

procedure StopProxy; cdecl;
begin
  try
    LockProxy;
    try
      if Assigned(Proxy) then
      begin
        Proxy.Stop;
        FreeAndNil(Proxy);
      end;
    finally
      UnlockProxy;
    end;
  except
    on E: Exception do
      WriteError('[StopProxy] %s: %s', [E.ClassName, E.Message]);
  end;
end;

exports
  InitProxy,
  StartProxy,
  StartProxyOnPort,
  StopProxy,
  AppendResource,
  RemoveResource;
{$endregion}

begin
end.

