unit Unit1;

{
The OmniRig_TLB.pas unit is required for this project. To generate this unit:

  - click on Project / Import Type Library in the Delphi IDE main menu;
  - click on the Add button and select the OmniRig.exe file in the file dialog;
  - enter your project's folder in the Unit Dir Name field;
  - click on the Create Unit button.
}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OmniRig_TLB, StdCtrls, Spin, ExtCtrls, Menus, wTime, uhook,
  Registry, uconsole, udp, URElays, uhotkey, variantutils, uqueue;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    SlowTimer: TTimer;
    Panel1: TPanel;
    Panel2: TPanel;
    HotCatcher1: THotCatcher;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Exit1Click(Sender: TObject);
    procedure SlowTimerTimer(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
    procedure HotCatcher1Hotkey(Sender: TObject; UID, Modifier,
      VirtualKey: Integer);
  private
    procedure StatusChangeEvent(Sender: TObject; RigNumber: Integer);
    procedure ParamsChangeEvent(Sender: TObject; RigNumber, Params: Integer);
    procedure RigTypeChangeEvent(Sender: TObject; RigNumber: Integer);

    procedure WmKeyboard(var Msg: TMessage); message WM_HOOK_KEY;
    Function  IsPTTActive:boolean;
    procedure SetPTT(enabled:boolean);
    procedure CreateRigControl;
    procedure KillRigControl;

    procedure AdjustDown;
    procedure DNRCheck;
    procedure CustomREply(Sender: TObject; RigNumber: Integer; Command: OleVariant; Reply: OleVariant);
  protected
    prevStatusMsg:string;
    reg:TRegistry;
    PMTxValidUntil:Int64;
    PMTxOn:boolean;
    PreviousFreq:Int64;
    InhibitTXUntil:Int64;
    RelayThread:trelays;
    catcher:THotCatcher;
    que_preamp:TQueue;
    preamp_deadline:Int64;
    procedure SavePos;
  public
    OmniRig: TOmniRigX;
    PTTUnti:Int64;
    PTTDown:boolean;
    DNRCheckDeadline:Int64;
    DidIEnable:boolean;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}


const
  HOTKEY_ID = 1;
  VK_OEM_5 = $DC; // \ key
  AddHangTime = 200;
  Ourkey = '\Software\WNR\OmniPTT\';
  SlowInt = 300;
  FastInt = 200;

  CONST_RELAY = 'S2OD2';
  CONST_RELAY_NR = 1;

//------------------------------------------------------------------------------
//                  OmniRig object creation and destruction
//------------------------------------------------------------------------------
procedure TForm1.FormCreate(Sender: TObject);
var l,t:integer;
begin
// EnableCon;
  reg:=TRegistry.Create();
  try
   reg.RootKey := HKEY_CURRENT_USER;
   reg.OpenKey(Ourkey, false);
   l:=reg.ReadInteger('Left');
   t:=reg.readinteger('Top');

   if ((l <= screen.Width) and (t <= screen.Height))
   then begin
    self.Left:=l;
    self.top:=t;
   end;


  except
   FreeAndNil(reg);
  end;

  que_preamp:=TQueue.Create;

  CreateRigControl;
  StartKeyboardHook(WindowHandle);
  RelayThread:=TRelays.Create(CONST_RELAY, CONST_RELAY_NR);
  panel1.  DoubleBuffered:=true;
  panel2.  DoubleBuffered:=true;
  DoubleBuffered:=true;

  HotCatcher1.RegisterKey(1, MOD_CONTROL, VK_ADD);
  HotCatcher1.RegisterKey(2, MOD_CONTROL, VK_SUBTRACT);
  HotCatcher1.RegisterKey(3, MOD_CONTROL, VK_MULTIPLY);
  HotCatcher1.RegisterKey(4, MOD_CONTROL, VK_DIVIDE);

end;


procedure TForm1.CreateRigControl;
begin
  //create and configure the OmniRig object
  OmniRig := TOmniRigX.Create(Self);
  try
    OmniRig.Connect;
    //listen to OmniRig events
    OmniRig.OnRigTypeChange := RigTypeChangeEvent;
    OmniRig.OnStatusChange := StatusChangeEvent;
    OmniRig.OnParamsChange := ParamsChangeEvent;
    OmniRig.OnCustomReply := CustomREply;

    //Check OmniRig version: in this demo we want V.1.1 to 1.99
    if OmniRig.InterfaceVersion < $0101 then Abort;
    if OmniRig.InterfaceVersion > $0199 then Abort;

    //show rig type, current status, and parameters
    RigTypeChangeEvent(nil, 1);
    RigTypeChangeEvent(nil, 2);
    StatusChangeEvent(nil, 1);
    StatusChangeEvent(nil, 2);
    ParamsChangeEvent(nil, 1, 0);
    ParamsChangeEvent(nil, 2, 0);
  except
    KillRigControl;
    MessageDlg('Unable to create the Omnirig object', mtError, [mbOk], 0);
  end;
end;

procedure TForm1.AdjustDown;
begin
  if (self.PTTDown) then
  begin
   if (xGetTickCount >PTTUnti) then
   begin
     PTTUnti:=xGetTickCount+round(AddHangTime * 2.5)
   end else begin
     PTTUnti:=xGetTickCount+AddHangTime;
   end;
  end else PTTUnti := 0;
end;

procedure TForm1.WmKeyboard(var Msg: TMessage);
begin
  self.PTTDown := msg.WParam = 1;
  AdjustDown;
end;

procedure TForm1.KillRigControl;
begin
  try FreeAndNil(OmniRig); except end;
end;



//------------------------------------------------------------------------------
//                         OmniRig event handling
//------------------------------------------------------------------------------


procedure TForm1.ParamsChangeEvent(Sender: TObject; RigNumber,
  Params: Integer);
var statusMsg:string;
begin
  if OmniRig = nil then Exit;

  statusMsg:=OmniRig.Rig1.StatusStr;

  if statusMsg<>prevStatusMsg
   then begin
     Con(statusMsg);
     prevStatusMsg:=statusMsg;
   end;

  case RigNumber of
    1:
      if OmniRig.Rig1.Status = ST_ONLINE
        then
          //display frequency and mode
          begin
              if not Timer1.Enabled then Timer1.Enabled:=true;
          end
        else
          //if rig is not online, clear the fields
          begin
              if Timer1.Enabled then Timer1.Enabled:=false;
          end;
    end;
  end;



procedure TForm1.RigTypeChangeEvent(Sender: TObject; RigNumber: Integer);
begin
  if OmniRig = nil then Exit;
  //display the radio model
  case RigNumber of
    1: Caption := OmniRig.Rig1.RigType;
    end;
end;


procedure TForm1.StatusChangeEvent(Sender: TObject; RigNumber: Integer);
begin
  if OmniRig = nil then Exit;
  //display the status string
  case RigNumber of
    1: Form1.Caption := OmniRig.Rig1.StatusStr;
    end;
end;


procedure TForm1.DNRCheck;
var mode:RigStatusX;
begin
 if xGetTickCount> DNRCheckDeadline then
 begin
  DNRCheckDeadline:=xGetTickCount + 5000;

  mode:=OmniRig.Rig1.Mode;

   if (mode =  PM_DIG_U) or (mode = PM_DIG_L) then
   begin
     OmniRig.Rig1.SendCustomCommand('NR00;', 5, ';');
   end;

 end;

end;

Function TForm1.IsPTTActive:boolean;
begin
 if xGetTickCount>PMTxValidUntil then
 begin
  result:=OmniRig.Rig1.Tx = PM_TX;
  PMTxOn:=result;
  PMTxValidUntil:=xGetTickCount+200;
 end else result:=PMTxOn;

end;

procedure TForm1.SetPTT(enabled:boolean);
begin
 if (Enabled and (InhibitTXUntil<>0) and (InhibitTXUntil>xGetTickCount)) then exit;

 if enabled then
   OmniRig.Rig1.Tx := PM_TX
  else
   OmniRig.Rig1.Tx := PM_RX;

 PMTxOn:=enabled;
 PMTxValidUntil:=xGetTickCount+200;
end;



procedure TForm1.Timer1Timer(Sender: TObject);
var needed:boolean;
begin
  if (OmniRig = nil) or (OmniRig.Rig1.Status <> ST_ONLINE) then Exit;
  AdjustDown;
  needed:=(PTTUnti>xGetTickCount) and (PTTUnti - xGetTickCount < 2000);

  if (needed <> ispttActive) then
  begin
    if needed then
    begin
         SetPTT(true);
         DidIEnable:=IsPTTActive;
    end
    else if DidIEnable then begin
         SetPTT(false);
         DidIEnable:=false;
    end;
  end;

  if (IsPTTActive) then
  begin
    Panel1.Color:=clRed;
  end else begin
    Panel1.Color:=clBtnFace;
  end;

  if ((RelayThread.Status=0) and (RelayThread.RelayState>0)) then
    Panel2.Color:=clLime
  else
    Panel2.Color:=clBtnFace;

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
 if IsPTTActive then
  OmniRig.Rig1.Tx := PM_RX;

 StopKeyboardHook;
 Timer1.Enabled:=false;
 FreeAndNil(que_preamp);
end;

procedure TForm1.SavePos;
begin
  reg:=TRegistry.Create();
  reg.RootKey := HKEY_CURRENT_USER;
  reg.OpenKey(Ourkey, true);

  reg.WriteInteger('Left', self.left);
  reg.WriteInteger('Top', self.top);

  FreeAndNil(reg);
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
 SavePos;
 WindowState:=wsMinimized;
 CanClose:=true;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
 SavePos;
 Close;
end;

procedure TForm1.SlowTimerTimer(Sender: TObject);
var freq:Int64;
begin
 if OmniRig = nil then Exit;
 SlowTimer.Enabled:=false;


 if OmniRig.Rig1.Status = st_online then
 begin
  freq:=OmniRig.rig1.GetTxFrequency;
  if freq<>PreviousFreq then
  begin
   if SlowTimer.Interval<>FastInt then  SlowTimer.Interval:=FastInt;
   InhibitTXUntil:=xGetTickCount+700;
  end else
   if SlowTimer.Interval<>SlowInt then SlowTimer.Interval:=SlowInt;

  PreviousFreq:=freq;
  SendUDPInfo(freq div 10, '172.16.1.50', 12060);
//  DNRCheck;
 end else SlowTimer.Interval:=SlowInt;

 SlowTimer.Enabled:=true;
end;

procedure TForm1.Panel2Click(Sender: TObject);
var _on:boolean;
begin
 if RelayThread.status>=0 then
 begin
  _on:=(RelayThread.RelayState and (1 shl CONST_RELAY_NR-1)) <> 0;
  RelayThread.RequestRelayState(CONST_RELAY_NR, not _on );
 end;
end;


procedure TForm1.HotCatcher1Hotkey(Sender: TObject; UID, Modifier,
  VirtualKey: Integer);
begin
  case UID of
   1: begin
       preamp_deadline:=xGetTickCount+2000;
       que_preamp.PushBack(1);
       OmniRig.Rig1.SendCustomCommand('PA0;', 5, ';');
      end;
   2: begin
       preamp_deadline:=xGetTickCount+2000;
       que_preamp.PushBack(2);
       OmniRig.Rig1.SendCustomCommand('PA0;', 5, ';');
      end;
   3: begin
       Panel2Click(nil);
      end;
   4: begin
       OmniRig.Rig1.SendCustomCommand('NR0;', 5, ';');
      end;
  end;

end;

procedure TForm1.CustomREply(Sender: TObject; RigNumber: Integer; Command: OleVariant; Reply: OleVariant);
var cmd, rep:string;
    ecmd:cardinal;
    r:integer;
begin
 cmd:=OleVariantToAnsiString(Command);
 rep:=OleVariantToAnsiString(reply);


   if cmd = 'PA0;' then begin // Preamp
     if que_preamp.PopFront(ecmd) then begin
       if preamp_deadline>xGetTickCount then begin
       r:=strtoint(rep[4]);


       case ecmd of
        1:
        begin
         if r < 2 then begin
          OmniRig.Rig1.SendCustomCommand('PA0'+IntToStr(r+1)+';', 5, ';');
         end;
        end;

        2:
        begin
         if r > 0 then begin
           OmniRig.Rig1.SendCustomCommand('PA0'+IntToStr(r-1)+';', 5, ';');
         end;
        end;
       end;
       end;
     end;

       Con('%x <-', [OmniRig.Rig1.Mode]);


   end else if cmd = 'NR0;' then begin // NR
      if rep[4]='1'then
       OmniRig.Rig1.SendCustomCommand('NR00;', 5, ';')
      else
       OmniRig.Rig1.SendCustomCommand('NR01;', 5, ';');

   end;

end;

end.

