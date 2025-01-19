unit MainWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  SegmentDisplay, DateUtils, kexec, ecweather, simpleipc, process, Math,
  uiconst, FileUtil;

type

  TAppMode = (amNone, amDVD, amMIDI, amCryo);

  { TMainForm }

  TMainForm = class(TForm)
    Hour1: TSegmentDisplay;
    Hour2: TSegmentDisplay;
    CalendarDate: TLabel;
    Conditions: TLabel;
    ForecastTitle: TLabel;
    ForecastTemp: TLabel;
    ForecastOutlook: TLabel;
    Label1: TLabel;
    SysMode: TLabel;
    SleepTimer: TImage;
    IPCClient: TSimpleIPCClient;
    MPlayer: TProcess;
    AMixer: TProcess;
    Timidity: TProcess;
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
    FAppMode: TAppMode;
    FModeIndex: Integer;
    FMIDIList, FCryoList: TStringList;
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
    procedure SetVolume(dB: string);
    procedure PlayMIDI;
    procedure EnterCryosleep;
    procedure StopAllMedia;
  public

  end;

var
  MainForm: TMainForm;

implementation

const
  {$IFDEF DEBUG}
  CRYOSLEEP = '/home/kveroneau/Music/Cryosleep - Zero Beat Guaranteed/';
  {$ELSE}
  CRYOSLEEP = '/opt/Cryosleep/';
  {$ENDIF}

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  {$IFNDEF DEBUG}
  WindowState:=wsFullScreen;
  {$ENDIF}
  FAppMode:=amNone;
  SysMode.Caption:='Standing By...';
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
  AddLog('Scanning MIDI Music...');
  FMIDIList:=TStringList.Create;
  FindAllFiles(FMIDIList, '/opt/MIDI/Bach/', '*.mid', False);
  FindAllFiles(FMIDIList, '/opt/MIDI/Beethoven/', '*.mid', False);
  FindAllFiles(FMIDIList, '/opt/MIDI/Mozart/', '*.mid', False);
  AddLog(IntToStr(FMIDIList.Count)+' MIDI Files found.');
  AddLog('Scanning CryoSleep...');
  FCryoList:=TStringList.Create;
  FindAllFiles(FCryoList, CRYOSLEEP, '*.mp3', False);
  AddLog(IntToStr(FCryoList.Count)+' Cryo Files found.');
  FVoiceEnabled:=True;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  StopAllMedia;
  FMIDIList.Free;
  FCryoList.Free;
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
    '-': IPCClient.SendStringMessage(mtBrightness, '50');
    '+': IPCClient.SendStringMessage(mtBrightness, '4000');
    'q': SuspendSystem;
    'm': PlayMIDI;
    'c': EnterCryosleep;
    's': StopAllMedia;
    ' ': ToggleControl(SleepTimer);
    'v': FVoiceEnabled:=not FVoiceEnabled;
  end;
  if (Key > #47) and (Key < #58) then
    if Key = #48 then
      PlayDVD('10')
    else
      PlayDVD(Key);
  Key:=#0;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
var
  dt: TSystemTime;
begin
  UpdateClock;
  if not MPlayer.Running then
    MPlayer.Active:=False;
  if not Timidity.Running then
    Timidity.Active:=False;
  if SleepTimer.Visible then
  begin
    GetLocalTime(dt);
    if (dt.Hour = 3) and (dt.Minute = 0) then
    begin
      SleepTimer.Visible:=False;
      SuspendSystem;
      Exit;
    end;
  end;
  if (not MPlayer.Active) and (FAppMode = amDVD) then
  begin
    Inc(FModeIndex);
    PlayDVD(IntToStr(FModeIndex));
  end
  else if (not Timidity.Active) and (FAppMode = amMIDI) then
    PlayMIDI
  else if (not MPlayer.Active) and (FAppMode = amCryo) then
    EnterCryosleep;
  case FAppMode of
    amNone: SysMode.Caption:='Standing By...';
    amDVD: SysMode.Caption:='DVD Player';
    amMIDI: SysMode.Caption:='MIDI Music';
    amCryo: SysMode.Caption:='CryoSleep';
  else
    SysMode.Caption:='Undetermined.';
  end;
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
  StopAllMedia;
  AddLog('Putting the Laptop into Power saving mode...');
  Timer.Enabled:=False;
  IPCClient.SendStringMessage(mtSuspend, 'SUSPEND');
  Sleep(10000);
  Timer.Enabled:=True;
  UpdateClock;
  FVoiceEnabled:=False;
  UpdateWeather;
  FVoiceEnabled:=True;
  AddLog('Welcome Back!');
end;

procedure TMainForm.PlayDVD(titleId: string);
begin
  if (FAppMode <> amNone) or (FAppMode <> amDVD) then
    StopAllMedia;
  if MPlayer.Running or MPlayer.Active then
    MPlayer.Active:=False;
  MPlayer.Parameters.Clear;
  MPlayer.Parameters.Add('-fs');
  MPlayer.Parameters.Add('dvd://'+titleId);
  SetVolume('0');
  MPlayer.Active:=True;
  FAppMode:=amDVD;
  FModeIndex:=StrToInt(titleId);
end;

procedure TMainForm.SetVolume(dB: string);
begin
  AddLog('Setting volume level to '+dB+' decibles.');
  AMixer.Parameters.Strings[4]:=dB+'dB';
  AMixer.Active:=True;
  AMixer.WaitOnExit;
  AMixer.Active:=False;
end;

procedure TMainForm.PlayMIDI;
begin
  SetVolume('-9');
  if FAppMode = amMIDI then
  begin
    Timidity.Active:=False;
    Inc(FModeIndex);
    if FModeIndex > FMIDIList.Count-1 then
      FModeIndex:=0;
  end
  else
  begin
    FAppMode:=amMIDI;
    FModeIndex:=0;
  end;
  FVoiceEnabled:=False;
  Timidity.Parameters.Strings[1]:=FMIDIList.Strings[FModeIndex];
  Timidity.Active:=True;
end;

procedure TMainForm.EnterCryosleep;
begin
  SetVolume('-9');
  if FAppMode = amCryo then
  begin
    MPlayer.Active:=False;
    Inc(FModeIndex);
    if FModeIndex > FCryoList.Count-1 then
      FModeIndex:=0;
  end
  else
  begin
    FAppMode:=amCryo;
    FModeIndex:=0;
  end;
  FVoiceEnabled:=False;
  MPlayer.Parameters.Clear;
  MPlayer.Parameters.Add(FCryoList.Strings[FModeIndex]);
  MPlayer.Active:=True;
end;

procedure TMainForm.StopAllMedia;
begin
  AddLog('Stopping all media playback...');
  FAppMode:=amNone;
  if MPlayer.Running or MPlayer.Active then
    MPlayer.Active:=False;
  if Timidity.Running or Timidity.Active then
    Timidity.Active:=False;
end;

end.

