unit MainWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  SegmentDisplay, DateUtils, kexec, ecweather, simpleipc, process, Math,
  uiconst;

type

  { TMainForm }

  TMainForm = class(TForm)
    Hour1: TSegmentDisplay;
    Hour2: TSegmentDisplay;
    CalendarDate: TLabel;
    Conditions: TLabel;
    ForecastTitle: TLabel;
    ForecastTemp: TLabel;
    ForecastOutlook: TLabel;
    IPCClient: TSimpleIPCClient;
    MPlayer: TProcess;
    Temperature: TLabel;
    LogList: TListBox;
    Minute1: TSegmentDisplay;
    Minute2: TSegmentDisplay;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure TimerTimer(Sender: TObject);
  private
    FVoiceEnabled: boolean;
    FBrightness: Integer;
    function GetWifiState: boolean;
    procedure SetWifiState(AValue: boolean);
  private
    property WifiState: boolean read GetWifiState write SetWifiState;
    procedure UpdateClock;
    procedure UpdateWeather;
    procedure ToggleControl(ctrl: TControl);
    procedure AddLog(msg: string);
    procedure SuspendSystem;
    procedure PlayDVD(titleId: string);
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  {$IFNDEF DEBUG}
  WindowState:=wsFullScreen;
  {$ENDIF}
  FVoiceEnabled:=False;
  UpdateClock;
  AddLog('Laptop System started.');
  if WifiState then
    AddLog('Wifi currently available!')
  else
    AddLog('Sadly, there is no Wifi.');
  {$IFNDEF DEBUG}
  UpdateWeather;
  {$ENDIF}
  FVoiceEnabled:=True;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  {$IFDEF DEBUG}
  IPCClient.SendStringMessage(mtDaemonExit, 'exit');
  {$ENDIF}
end;

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  case Key of
    #27: Close;
    'd': ToggleControl(CalendarDate);
    'l': ToggleControl(LogList);
    'w': WifiState:=not WifiState;
    'W': UpdateWeather;
    'i': IPCClient.SendStringMessage(mtBrightness, '1000');
    'q': SuspendSystem;
  end;
  if (Key > #47) and (Key < #58) then
    if Key = #48 then
      PlayDVD('10')
    else
      PlayDVD(Key);
  Key:=#0;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  UpdateClock;
end;

function TMainForm.GetWifiState: boolean;
var
  f: TextFile;
  s: string;
begin
  AddLog('Determine the current state of the WiFi...');
  system.Assign(f, '/sys/class/net/'+WIFI_DEV+'/operstate');
  Reset(f);
  ReadLn(f, s);
  system.Close(f);
  if s = 'up' then
    Result:=True
  else
    Result:=False;
end;

procedure TMainForm.SetWifiState(AValue: boolean);
begin
  if GetWifiState = AValue then
    Exit;
  if AValue then
  begin
    AddLog('Turning on the WiFi...');
    Exec('/usr/bin/nmcli', 'radio wifi on');
  end
  else
  begin
    AddLog('Turning off the WiFi...');
    Exec('/usr/bin/nmcli', 'radio wifi off');
  end;
end;

procedure TMainForm.UpdateClock;
var
  dt: TSystemTime;
begin
  GetLocalTime(dt);
  Hour1.Digit:=Floor(dt.Hour div 10);
  Hour2.Digit:=dt.Hour mod 10;
  Minute1.Digit:=Floor(dt.Minute div 10);
  Minute2.Digit:=dt.Minute mod 10;
  CalendarDate.Caption:=FormatDateTime('dddd mmmm d, yyyy', Today);
end;

procedure TMainForm.UpdateWeather;
var
  dis: boolean;
begin
  if not WifiState then
  begin
    dis:=True;
    WifiState:=True;
    repeat
      AddLog('Waiting for WiFi...');
      Sleep(1000);
    until WifiState;
  end
  else
    dis:=False;
  if not Assigned(weather_cache) then
  begin
    AddLog('Initializing Weather Cache...');
    weather_cache:=CalgaryWeather.Create;
  end;
  with weather_cache do
  begin
    Conditions.Caption:=weather^.conditions;
    Temperature.Caption:=weather^.temperature;
    with weather^.forecast[0] do
    begin
      ForecastTitle.Caption:=title;
      ForecastTemp.Caption:=temperature;
      ForecastOutlook.Caption:=outlook;
    end;
  end;
  if dis then
    WifiState:=False;
end;

procedure TMainForm.ToggleControl(ctrl: TControl);
begin
  ctrl.Visible:=not ctrl.Visible;
end;

procedure TMainForm.AddLog(msg: string);
begin
  LogList.ItemIndex:=LogList.Items.Add(DateToISO8601(Now)+' | '+msg);
  LogList.MakeCurrentVisible;
  Application.ProcessMessages;
  if FVoiceEnabled then
    Exec('/usr/bin/espeak', '"'+msg+'"');
end;

procedure TMainForm.SuspendSystem;
begin
  AddLog('Putting the Laptop into Power saving mode...');
  Timer.Enabled:=False;
  IPCClient.SendStringMessage(mtSuspend, 'SUSPEND');
  Sleep(10000);
  Timer.Enabled:=True;
  UpdateClock;
  UpdateWeather;
  AddLog('Welcome Back!');
end;

procedure TMainForm.PlayDVD(titleId: string);
begin
  if MPlayer.Running or MPlayer.Active then
    MPlayer.Active:=False;
  MPlayer.Parameters.Strings[0]:='dvd://'+titleId;
  MPlayer.Active:=True;
end;

end.

