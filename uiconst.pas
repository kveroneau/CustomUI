unit uiconst;

{$mode ObjFPC}{$H+}

interface

const
  {$IFDEF DEBUG}
  WIFI_DEV = 'wlo1';
  {$ELSE}
  WIFI_DEV = 'wlp4s0';
  {$ENDIF}

  mtDaemonExit = 9364;
  mtBrightness = 3750;
  mtSuspend = 8362;

implementation

end.

