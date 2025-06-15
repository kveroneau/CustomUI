unit MainWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  SegmentDisplay, DateUtils, kexec, ecweather, simpleipc, process, Math,
  uiconst, FileUtil, fphttpclient, BaseUnix, ssockets, forecast, suncalc;

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
    RepeatSong: TImage;
    Label1: TLabel;
    SongLabel: TLabel;
    SongTitle: TLabel;
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
    FVoiceEnabled, FExternalDisplay: boolean;
    FAppMode: TAppMode;
    FModeIndex: Integer;
    FMIDIList, FCryoList: TStringList;
    FSunrise, FSunset: TDateTime;
    function GetWifiState: boolean;
    procedure SetWifiState(AValue: boolean);
  private
    property WifiState: boolean read GetWifiState write SetWifiState;
    procedure UpdateClock;
    procedure UpdateWeather;
    procedure ToggleControl(ctrl: TControl);
    procedure SetSongTitle(ATitle: string);
    procedure AddLog(msg: string);
    procedure SuspendSystem;
    procedure PlayDVD(titleId: string);
    procedure SetVolume(dB: string);
    procedure PlayMIDI;
    procedure EnterCryosleep;
    procedure StopAllMedia;
    procedure ToggleHDMI;
    procedure PowerOff;
    procedure UpdateApp;
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
  CalcSunriseSet(51.0486, -114.0708, -6, FSunrise, FSunset);
  FAppMode:=amNone;
  SysMode.Caption:='Standing By...';
  FVoiceEnabled:=False;
  FExternalDisplay:=False;
  UpdateClock;
  AddLog('Laptop System v'+SYS_VER+' started.');
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
    {$IFNDEF DEBUG}
    'w': WifiState:=not WifiState;
    'q': SuspendSystem;
    'Q': PowerOff;
    't': ToggleHDMI;
    'e': Exec('/usr/bin/eject','');
    '-': IPCClient.SendStringMessage(mtBrightness, '50');
    '+': IPCClient.SendStringMessage(mtBrightness, '4000');
    {$ENDIF}
    'W': UpdateWeather;
    'm': PlayMIDI;
    'c': EnterCryosleep;
    's': StopAllMedia;
    ' ': ToggleControl(SleepTimer);
    'v': FVoiceEnabled:=not FVoiceEnabled;
    ',': SetVolume('-9');
    '.': SetVolume('0');
    'r': ToggleControl(RepeatSong);
    'U': UpdateApp;
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
  system.Assign(f, '/sys/class/net/'+WIFI_DEV+'/operstate');
  WriteLn('/sys/class/net/'+WIFI_DEV+'/operstate');
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
    try
      weather_cache:=CalgaryWeather.Create;
    except
      AddLog('Failed to initialize the weather cache.');
      weather_cache:=Nil;
      Exit;
    end;
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

procedure TMainForm.SetSongTitle(ATitle: string);
begin
  if ATitle = 'Nil' then
  begin
    SongLabel.Visible:=False;
    SongTitle.Visible:=False;
  end
  else
  begin
    SongLabel.Visible:=True;
    SongTitle.Visible:=True;
    SongTitle.Caption:=ATitle;
  end;
end;

procedure TMainForm.AddLog(msg: string);
begin
  LogList.ItemIndex:=LogList.Items.Add(DateToISO8601(Now)+' | '+msg);
  LogList.MakeCurrentVisible;
  Application.ProcessMessages;
  {$IFNDEF DEBUG}
  if FVoiceEnabled then
    Exec('/usr/bin/espeak', '"'+msg+'"');
  {$ENDIF}
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
  CalcSunriseSet(51.0486, -114.0708, -6, FSunrise, FSunset);
  if DateTimeToFileDate(Now) > DateTimeToFileDate(FSunset) then
    IPCClient.SendStringMessage(mtBrightness, '50')
  else
    IPCClient.SendStringMessage(mtBrightness, '4000');
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
    if not RepeatSong.Visible then
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
  SetSongTitle(ExtractFileName(FMIDIList.Strings[FModeIndex]));
end;

procedure TMainForm.EnterCryosleep;
begin
  SetVolume('-9');
  if FAppMode = amCryo then
  begin
    MPlayer.Active:=False;
    if not RepeatSong.Visible then
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
  SetSongTitle(ExtractFileName(FCryoList.Strings[FModeIndex]));
end;

procedure TMainForm.StopAllMedia;
begin
  AddLog('Stopping all media playback...');
  FAppMode:=amNone;
  if MPlayer.Running or MPlayer.Active then
    MPlayer.Active:=False;
  if Timidity.Running or Timidity.Active then
    Timidity.Active:=False;
  SetSongTitle('Nil');
end;

procedure TMainForm.ToggleHDMI;
var
  i: Integer;
begin
  if FExternalDisplay then
  begin
    Exec('/usr/bin/xrandr', '--output LVDS-1 --auto');
    Exec('/usr/bin/xrandr', '--output HDMI-1 --off');
    MPlayer.Environment.Clear;
    FExternalDisplay:=False;
  end
  else
  begin
    Exec('/usr/bin/xrandr', '--output HDMI-1 --auto');
    Exec('/usr/bin/xrandr', '--output LVDS-1 --off');
    for i:=0 to GetEnvironmentVariableCount-1 do
      MPlayer.Environment.Add(GetEnvironmentString(i));
    MPlayer.Environment.Add('ALSA_CARD=1');
    FExternalDisplay:=True;
  end;
end;

procedure TMainForm.PowerOff;
begin
  IPCClient.SendStringMessage(mtPowerOff, 'POWEROFF');
  Close;
end;

procedure TMainForm.UpdateApp;
var
  f: TMemoryStream;
  r: Integer;
begin
  if not WifiState then
  begin
    AddLog('Wifi not enabled, cannot update without connection.');
    Exit;
  end;
  AddLog('Attempting to Update the system...');
  with TFPHTTPClient.Create(Nil) do
    try
      f:=TMemoryStream.Create;
      try
        Get(UPDATE_URL, f);
      except
        On ESocketError do
        begin
          AddLog('Update failed, socket error.');
          Exit;
        end;
      end;
      f.SaveToFile(GetEnvironmentVariable('HOME')+'/LaptopSystem');
      FpChmod(GetEnvironmentVariable('HOME')+'/LaptopSystem', &500);
      AddLog('Download complete, attempting to start.');
      Hide;
      Application.ProcessMessages;
      r:=Exec(GetEnvironmentVariable('HOME')+'/LaptopSystem', '');
      Show;
      Application.ProcessMessages;
      FVoiceEnabled:=True;
      AddLog('Welcome back to the original application.');
    finally
      f.Free;
      Free;
    end;
end;

end.

