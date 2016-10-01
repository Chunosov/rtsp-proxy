program RtspProxy;

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes;

const
  RtspProxyLib = 'RtspProxyLib';

procedure InitProxy; stdcall; external RtspProxyLib;
procedure StartProxy; stdcall; external RtspProxyLib;
procedure StopProxy; stdcall; external RtspProxyLib;
procedure AppendResource(Name, RtspAddress: PChar); stdcall; external RtspProxyLib;

procedure LoadResources;
const
  ResourceFile = 'Resources.cfg';
var
  I, P: Integer;
  Lines: TStrings;
  Line, Name, Resource: String;
begin
  if FileExists(ResourceFile) then
  begin
    Lines := TStringList.Create;
    try
      Lines.LoadFromFile(ResourceFile);
      for I := 0 to Lines.Count-1 do
      begin
        Line := Trim(Lines[I]);
        if Line = '' then Continue;
        if Line[1] = '#' then Continue;
        P := Pos('=', Line);
        if P < 0 then Continue;
        Name := Trim(Copy(Line, 1, P-1));
        Resource := Trim(Copy(Line, P+1, MaxInt));
        AppendResource(PChar(Name), PChar(Resource));
      end;
    finally
      Lines.Free;
    end;
  end;
end;

begin
  try
    WriteLn('MWIE RTSP Proxy Server. Signatec (c) 2012.');
    WriteLn;
    WriteLn('Press Enter to stop proxy');
    WriteLn;

    InitProxy;
    LoadResources;
    StartProxy;
    ReadLn;
    StopProxy;
  except
    on E: Exception do WriteLn(E.ClassName + ': ' + E.Message);
  end;
end.
