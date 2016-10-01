unit Log;

interface

uses
  Console, Windows;

const
  ServerColor = Console.Yellow;
  ClientColor = Console.LightGreen;
  ClientProxyColor = Console.Green;
  ServerProxyColor = Console.Brown;
  CRLF = #$D#$A; 

procedure WriteLine(const Msg: String); overload;
procedure WriteLine(const Msg: String; const Args: array of const); overload;
procedure WriteLine(const Msg: String; Color: Byte); overload;
procedure WriteLine(const Msg: String; const Args: array of const; Color: Byte); overload;
procedure WriteError(const Msg: String); overload;
procedure WriteError(const Msg: String; const Args: array of const); overload;

procedure WriteToLogFile(const Msg: String);
procedure WriteToConsole(const Msg: String; Color: Byte);

var
  LogToConsole: Boolean = False;
  LogToFile: Boolean = False;
  LogFileName: String = 'RtspProxy.log';

implementation

uses
  SysUtils, Classes;

var
  ConcoleLocker: TRTLCriticalSection;
  ConcoleLockerInited: Boolean = False;

procedure Lock;
begin
  if not ConcoleLockerInited then
  begin
    InitializeCriticalSection(ConcoleLocker);
    ConcoleLockerInited := True;
  end;
  EnterCriticalSection(ConcoleLocker);
end;

procedure Unlock;
begin
  LeaveCriticalSection(ConcoleLocker);
end;

procedure WriteToLogFile(const Msg: String);
var
  S: String;
  LogFile: THandle;
begin
  S := Format('[%s] %s'#13#10, [FormatDateTime('dd-mm-yyyy hh:mm:ss:zzz', Now), Msg]);
  if FileExists(LogFileName) then
  begin
    LogFile := FileOpen(LogFileName, fmOpenWrite);
    FileSeek(LogFile, 0, soFromEnd);
  end
  else
    LogFile := FileCreate(LogFileName);
  FileWrite(LogFile, PChar(S)^, Length(S));
  FileClose(LogFile);
end;

procedure WriteToConsole(const Msg: String; Color: Byte);
var
  OldColor: Byte;
begin
  OldColor := TextColor;
  TextColor(Color);
  WriteLn(Msg);
  TextColor(OldColor);
end;

procedure WriteLine(const Msg: String);
begin
  Lock;
  try
    if LogToConsole then
      WriteLn(Msg);

    if LogToFile then
      WriteToLogFile(Msg);
  finally
    Unlock;
  end;
end;

procedure WriteLine(const Msg: String; const Args: array of const);
begin
  WriteLine(Format(Msg, Args));
end;

procedure WriteLine(const Msg: String; Color: Byte);
begin
  Lock;
  try
    if LogToConsole then
      WriteToConsole(Msg, Color);

    if LogToFile then
      WriteToLogFile(Msg);
  finally
    Unlock;
  end;
end;

procedure WriteLine(const Msg: String; const Args: array of const; Color: Byte);
begin
  WriteLine(Format(Msg, Args), Color);
end;

procedure WriteError(const Msg: String);
begin
  WriteLine('ERROR: ' + Msg, Console.LightRed);
end;

procedure WriteError(const Msg: String; const Args: array of const);
begin
  WriteError(Format(Msg, Args));
end;

initialization

finalization
  if ConcoleLockerInited then
    DeleteCriticalSection(ConcoleLocker);

end.
