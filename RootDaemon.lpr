program RootDaemon;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp, simpleipc, kexec, uiconst, ksignals, klogger,
  BaseUnix;

type

  { TRootDaemon }

  TRootDaemon = class(TCustomApplication)
  private
    FServer: TSimpleIPCServer;
    procedure HandleSignal;
    procedure InstallService;
    procedure WriteSetting(fname, data: string);
    procedure SetBrightness(bri: string);
    procedure ProcessIPC(Sender: TObject);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure RunServer;
  end;

const
  SYSTEMCTL = '/usr/bin/systemctl';

{ TRootDaemon }

procedure TRootDaemon.HandleSignal;
var
  ipc: TSimpleIPCClient;
begin
  LogInfo('TERM signal caught, stopping daemon...');
  ipc:=TSimpleIPCClient.Create(Nil);
  try
    ipc.ServerID:='rootipc';
    ipc.Active:=True;
    ipc.SendStringMessage(mtDaemonExit, 'exit');
    ipc.Active:=False;
  finally
    ipc.Free;
  end;
end;

procedure TRootDaemon.InstallService;
var
  r: TResourceStream;
begin
  if not FileExists('/usr/local/sbin/RootDaemon') then
    raise Exception.Create('Please install daemon binary first!');
  WriteLn(' * Performing Service Installation...');
  r:=TResourceStream.Create(HINSTANCE, 'SERVICE', RT_RCDATA);
  try
    r.SaveToFile('/etc/systemd/system/rootdaemon.service');
  finally
    r.Free;
  end;
  Exec(SYSTEMCTL, 'daemon-reload');
  Exec(SYSTEMCTL, 'enable rootdaemon.service');
  Exec(SYSTEMCTL, 'start rootdaemon.service');
end;

procedure TRootDaemon.WriteSetting(fname, data: string);
var
  f: Text;
begin
  system.Assign(f, fname);
  Rewrite(f);
  Write(f, data);
  system.Close(f);
end;

procedure TRootDaemon.SetBrightness(bri: string);
begin
  WriteSetting('/sys/class/backlight/intel_backlight/brightness', bri);
end;

procedure TRootDaemon.ProcessIPC(Sender: TObject);
begin
  case FServer.MsgType of
    mtBrightness: SetBrightness(FServer.StringMessage);
    mtSuspend: Exec('/usr/bin/systemctl', 'suspend');
    mtDaemonExit: FServer.Active:=False;
  end;
end;

procedure TRootDaemon.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hi', 'help install');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('i', 'install') then
  begin
    InstallService;
    Terminate;
    Exit;
  end;

  {$IFDEF DEBUG}
  SetupLog('/tmp/daemon.log');
  {$ELSE}
  SetupLog('/var/log/RootDaemon.log');
  {$ENDIF}
  LogInfo('Root Control Daemon started.');
  OnSignal:=@HandleSignal;
  FpUmask(0);

  RunServer;
  LogInfo('Root Control Daemon shutdown successfully.');

  // stop program loop
  Terminate;
end;

constructor TRootDaemon.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TRootDaemon.Destroy;
begin
  inherited Destroy;
end;

procedure TRootDaemon.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h -i');
end;

procedure TRootDaemon.RunServer;
begin
  FServer:=TSimpleIPCServer.Create(Nil);
  try
    FServer.ServerID:='rootipc';
    FServer.Global:=True;
    FServer.OnMessage:=@ProcessIPC;
    FServer.Active:=True;
    repeat
      repeat
        Sleep(1000);
      until FServer.PeekMessage(-1, True);
    until not FServer.Active;
  finally
    FServer.Free;
  end;
end;

var
  Application: TRootDaemon;

{$R *.res}

begin
  Application:=TRootDaemon.Create(nil);
  Application.Title:='Root Daemon';
  Application.Run;
  Application.Free;
end.

