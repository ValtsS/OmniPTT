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
  OmniRig_TLB, StdCtrls, Spin, ExtCtrls, Menus, wTime, uhook, jwaWinsock2,
  Registry, uconsole, udp, uhotkey, variantutils, uqueue, udnslookup,
  ustrlist, wsocks, Math, finputfreq, frb;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    SlowTimer: TTimer;
    Panel1: TPanel;
    Panel2: TPanel;
    HotCatcher1: THotCatcher;
    ErrTxt: TLabel;
    Misc: TTimer;
    PopupMenu1: TPopupMenu;
    N21: TMenuItem;
    N11: TMenuItem;
    Real1: TMenuItem;
    BoG11: TMenuItem;
    BoG21: TMenuItem;
    BoG31: TMenuItem;
    BoG451: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Exit1Click(Sender: TObject);
    procedure SlowTimerTimer(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
    procedure HotCatcher1Hotkey(Sender: TObject; UID, Modifier,
      VirtualKey: Integer);
    procedure MiscTimer(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure N11Click(Sender: TObject);
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
    procedure LookupDone(hostent:PHostEnt; msgrec:TMessage); virtual;
  protected
    prevStatusMsg:string;
    reg:TRegistry;
    PMTxValidUntil:Int64;
    PMTxOn:boolean;
    PreviousFreq:Int64;
    InhibitTXUntil:Int64;
    catcher:THotCatcher;
    que_preamp:TQueue;
    que_dnr:TQueue;
    preamp_deadline:Int64;
    dnr_deadline:Int64;
    DNS:TDNSLookup;
    rpi_hostip:string;
    rpi_hostage:Int64;

    deadline_level:Int64;
    deadline_span:Int64;
    deadline_mode:Int64;
    deadline_slit:Int64;

    current_level:double;
    current_span:integer;
    current_mode, prev_mode:integer;
    prev_volume:string;
    freqInput:TFreqInputForm;
    relays2:TRelays2;

    procedure SavePos;
    procedure Lookup;

    procedure TuneFreq(Delta:Int64);
    procedure NewFreqRequested(freq:Integer);
    procedure SelectAntenna;
    procedure TriggerRX(old,new:Int64);

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
  VK_OEM_3 = $C0;
  VK_OEM_5 = $DC; // \ key
  AddHangTime = 200;
  Ourkey = '\Software\WNR\OmniPTT\';
  SlowInt = 300;
  FastInt = 200;

  CONST_RELAY = 'S2OD2';
  CONST_RELAY_NR = 1;

  RPI = 'amp.wnrsoft.lv';

  CONST_COM = 7;

var RelAltStatus:integer = 1;


procedure TForm1.FormCreate(Sender: TObject);
var l,t:integer;
begin
// EnableCon;
  DNS:=TDNSLookup.create;
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

  self.Width:=170;
  self.height:=60;

  relays2:=TRelays2.Create(CONST_COM);

  que_preamp:=TQueue.Create;
  que_dnr:=TQueue.Create;
  prev_volume:='010';
  CreateRigControl;
  StartKeyboardHook(WindowHandle);
  panel1.  DoubleBuffered:=true;
  panel2.  DoubleBuffered:=true;
  DoubleBuffered:=true;

  HotCatcher1.RegisterKey(1, MOD_CONTROL, VK_ADD); // Preamp increase
  HotCatcher1.RegisterKey(2, MOD_CONTROL, VK_SUBTRACT); // Preamp decrease
  HotCatcher1.RegisterKey(3, MOD_CONTROL, VK_MULTIPLY); // Toggle panorama
  HotCatcher1.RegisterKey(4, MOD_CONTROL, VK_DIVIDE);  // Toggle DNR
  HotCatcher1.RegisterKey(5, 0, VK_DIVIDE);   // Swap VFO A/B
  HotCatcher1.RegisterKey(6, MOD_CONTROL, VK_NUMPAD0); // Toggle DNF
  HotCatcher1.RegisterKey(7, 0, VK_OEM_3);   // Swap VFO A/B

  HotCatcher1.RegisterKey(10, 0, VK_NUMPAD6); // Active VFO +5KHz
  HotCatcher1.RegisterKey(11, 0, VK_NUMPAD4); // Active VFO -5kHz

  HotCatcher1.RegisterKey(8, MOD_ALT, VK_NUMPAD6); // Active VFO +5KHz
  HotCatcher1.RegisterKey(9, MOD_ALT, VK_NUMPAD4); // Active VFO -5kHz

  HotCatcher1.RegisterKey(12, MOD_CONTROL, VK_NUMPAD6); // Secondary VFO +5kHz
  HotCatcher1.RegisterKey(13, MOD_CONTROL, VK_NUMPAD4); // Secondary VFO -5kHz

  HotCatcher1.RegisterKey(14, 0, VK_NUMPAD5); // Round active VFO freqy
  HotCatcher1.RegisterKey(15, MOD_CONTROL, VK_DECIMAL); // Toggle split mode


  HotCatcher1.RegisterKey(20, 0, VK_NUMPAD1); // SSB
  HotCatcher1.RegisterKey(22, 0, VK_NUMPAD3); // DATA

  t:=VK_F1;
  for l:=30 to 40 do begin
   HotCatcher1.RegisterKey(l, MOD_SHIFT or MOD_CONTROL, t); // bands 160 to 6m
   inc(t);
  end;

  HotCatcher1.RegisterKey(50, MOD_CONTROL, VK_NUMPAD8); // waterfall gain up +3dB
  HotCatcher1.RegisterKey(51, MOD_CONTROL, VK_NUMPAD2); // waterfall gain down -3dB

  HotCatcher1.RegisterKey(52, 0, VK_NUMPAD8); // zoom in
  HotCatcher1.RegisterKey(53, 0, VK_NUMPAD2); // zoom out

  HotCatcher1.RegisterKey(60, 0, VK_NUMPAD7); // move view left
  HotCatcher1.RegisterKey(61, 0, VK_NUMPAD9); // move view right


  HotCatcher1.RegisterKey(54, 0, VK_MULTIPLY); // toggle scope


  HotCatcher1.RegisterKey(70, MOD_SHIFT or MOD_CONTROL, ord('M')); // mute/unmute


  HotCatcher1.RegisterKey(80,  MOD_SHIFT or MOD_CONTROL, VK_F12); // manual freq
  HotCatcher1.RegisterKey(81,  MOD_ALT, VK_OEM_3); // Antenna switch popup
  HotCatcher1.RegisterKey(82,  MOD_CONTROL, VK_OEM_3); // Antenna switch to previous mode

  freqInput:=TFreqInputForm.Create(self);
  freqInput.OnFreqChange:=NewFreqRequested;
end;



procedure TForm1.Lookup;
begin
  if xGetTickCount>rpi_hostage then begin
   rpi_hostage:=xGetTickCount+15000;
   DNS.QueueLookup(LookupDone, RPI);
  end;
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

procedure TForm1.LookupDone(hostent:PHostEnt; msgrec:TMessage);
var addres:istrlist;
    err:integer;
begin

 err:=MsgRec.LParamHi;

 if (err=0) then
 begin
  addres:=GetIPList(hostent);

   if addres.count>0 then begin

    rpi_hostip:=addres[0];
    Con(rpi_hostip);
   end else Con('Returned no IP addres...');

 end else Con('DNS error %d',[err]);


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

   if (mode =  PM_DIG_U) {or (mode = PM_DIG_L)} then
   begin
       dnr_deadline:=xGetTickCount+2000;
       que_dnr.PushBack(3);
       OmniRig.Rig1.SendCustomCommand('NR0;', 0, '');
   end;

 end;

 if xGetTickCount > deadline_slit then begin
   if Omnirig.Rig1.Split = PM_SPLITON then begin
     if Abs(Omnirig.Rig1.FreqA-Omnirig.Rig1.FreqB)>200*1000 then begin
      Omnirig.Rig1.Split := PM_SPLIToFF;
     end;
   end;

   deadline_slit:=xGetTickCount+2700;
 end;

 if xGetTickCount > deadline_level then begin
   OmniRig.Rig1.SendCustomCommand('SS04;', 0, '');
   deadline_level:=xGetTickCount+10000;
 end else if xGetTickCount > deadline_span  then begin
 OmniRig.Rig1.SendCustomCommand('SS05;', 0, '');
   deadline_span:=xGetTickCount+9000;
 end else if xGetTickCount > deadline_mode then begin
 OmniRig.Rig1.SendCustomCommand('SS06;', 0, '');
   deadline_mode:=xGetTickCount+8000;
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


Function AntName(i:integer):string;
begin
  case i of
    0: result:='Real';
    1: result:='Real-3dB';
    2: result:='LoG';
    3: result:='BoG-1';
    4: result:='BoG-2';
    5: result:='BoG-3';
    6: result:='BoG-4';

   else
    result:='??';
  end;

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

  if ((relays2.Status>0)) then
    Panel2.Color:=clLime
  else
    Panel2.Color:=clBtnFace;

  Panel2.Caption:=AntName(relays2.status);

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
 if IsPTTActive then
  OmniRig.Rig1.Tx := PM_RX;

 StopKeyboardHook;
 Timer1.Enabled:=false;
 FreeAndNil(que_preamp);
 FreeAndNil(que_dnr);
 if Assigned(DNS) then FreeAndNil(DNS);
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
 if Assigned(DNS) then DNS.AbortAll;
 SavePos;
 WindowState:=wsMinimized;
 CanClose:=true;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
 SavePos;
 Close;
end;

procedure TForm1.TriggerRX(old,new:Int64);
begin

 if (relays2.Status>1) and (new>15e6) then begin
    relays2.Request(0);
 end;

end;

procedure TForm1.SlowTimerTimer(Sender: TObject);
const errMsg = 'No IP address for AMP!';
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

  triggerRx(PreviousFreq, freq);

  PreviousFreq:=freq;
  if rpi_hostip<>'' then begin
   SendUDPInfo(freq div 10, rpi_hostip, 12060);
   if ErrTxt.Caption = errMsg then
    ErrTxt.Caption:='';
  end  else begin
   ErrTxt.Caption:=errMsg;
  end;
  DNRCheck;
 end else SlowTimer.Interval:=SlowInt;

 SlowTimer.Enabled:=true;
end;

procedure TForm1.Panel2Click(Sender: TObject);
var _on:boolean;
begin

  if relays2.Status=0 then
  begin
    if PreviousFreq>5.5e6 then
     relays2.Request(1)
    else
     relays2.Request(2);
  end else
   relays2.Request(0);
end;


Function SpanToHz(span:integer):integer;
begin
  result:=0;
  case span of
   0: result:=1000;
   1: result:=2*1000;
   2: result:=5*1000;
   3: result:=10*1000;
   4: result:=20*1000;
   5: result:=50*1000;
   6: result:=100*1000;
   7: result:=200*1000;
   8: result:=500*1000;
   9: result:=1000*1000;
  end;
end;

procedure TForm1.HotCatcher1Hotkey(Sender: TObject; UID, Modifier,
  VirtualKey: Integer);

 procedure SetSSB;
 begin
           if OmniRig.Rig1.GetRxFrequency<10e6 then begin
             OmniRig.Rig1.SendCustomCommand('MD01;',0,'');
             OmniRig.Rig1.SendCustomCommand('MD11;',0,'');
           end else begin
            OmniRig.Rig1.SendCustomCommand('MD02;',0,'');
            OmniRig.Rig1.SendCustomCommand('MD12;',0,'');
           end;
 end;

var tmp, tmp2, f1, f2:integer;
begin
  case UID of
   1: begin
       preamp_deadline:=xGetTickCount+2000;
       que_preamp.PushBack(1);
       OmniRig.Rig1.SendCustomCommand('PA0;', 0, '');
      end;
   2: begin
       preamp_deadline:=xGetTickCount+2000;
       que_preamp.PushBack(2);
       OmniRig.Rig1.SendCustomCommand('PA0;', 0, '');
      end;
   3: begin
       Panel2Click(nil);
      end;
   4: begin
       dnr_deadline:=xGetTickCount+2000;
       que_dnr.PushBack(1);
       OmniRig.Rig1.SendCustomCommand('NR0;', 0, '');
      end;
   5,7: begin
       OmniRig.Rig1.SendCustomCommand('SV;', 0, '');
      end;
   6: begin
       OmniRig.Rig1.SendCustomCommand('BC0;', 0, '');
      end;

   8,10: begin
           if uid=10 then tmp:=5000 else tmp:=500;
           TuneFreq(tmp);
       end;

   9,11: begin
           if uid=11 then tmp:=5000 else tmp:=500;
           TuneFreq(-tmp);
          end;
   12: begin
            case OmniRig.Rig1.Vfo of
              PM_VFOA,
              PM_VFOAA, PM_VFOBA:
                       begin
                            OmniRig.Rig1.freqA:= OmniRig.Rig1.freqA+5000;
                       end;
              PM_VFOB,
              PM_VFOAB, PM_VFOBB:
                       begin
                            OmniRig.Rig1.freqB:= OmniRig.Rig1.freqB+5000;
                       end;
             end;
       end;

   13: begin
            case OmniRig.Rig1.Vfo of
              PM_VFOA,
              PM_VFOAA, PM_VFOBA:
                       begin
                            OmniRig.Rig1.freqA:= OmniRig.Rig1.freqA-5000;
                       end;
              PM_VFOB,
              PM_VFOAB, PM_VFOBB:
                       begin
                            OmniRig.Rig1.freqB:= OmniRig.Rig1.freqB-5000;
                       end;
             end;
       end;


   14: begin
            case OmniRig.Rig1.Vfo of
              PM_VFOA,
              PM_VFOAA, PM_VFOAB:
                       begin
                            OmniRig.Rig1.freqA:= round(OmniRig.Rig1.freqA/1000.0)*1000;
                       end;
              PM_VFOB,
              PM_VFOBA, PM_VFOBB:
                       begin
                            OmniRig.Rig1.freqB:= round(OmniRig.Rig1.freqB/1000.0)*1000;
                       end;
             end;


             if current_mode>=0 then begin
              OmniRig.Rig1.SendCustomCommand('SS06'+inttohex(current_mode,1)+'0000;',0,'');
              deadline_span:=0;
             end


       end;
    15: begin
             if Omnirig.Rig1.Split = PM_SPLITON
              then Omnirig.Rig1.Split := PM_SPLITOFF
             else begin
              Omnirig.Rig1.SetSplitMode(OmniRig.Rig1.GetRxFrequency, OmniRig.Rig1.GetRxFrequency+5000);
              SetSSB;
              OmniRig.Rig1.SendCustomCommand('SS0670000;',0,'');
              OmniRig.Rig1.SendCustomCommand('SS0570000;',0,'');
             end;

        end;

     20: begin
           SetSSB;
          end;
     22: begin
           OmniRig.Rig1.SendCustomCommand('MD0C;',0,'');
           OmniRig.Rig1.SendCustomCommand('MD1C;',0,'');
         end;

     30..40: begin
           OmniRig.Rig1.SendCustomCommand('BS'+Format('%.2d;',[UID-30]),0,'');
        end;

      50..51: begin
          if UID=50 then
            current_level:=Min(12,current_level+3)
           else
            current_level:=Max(-12,current_level-3);

           OmniRig.Rig1.SendCustomCommand('SS04'+FormatFloat('+00.0;-00.0;+00.0', current_level)+';',0,'');
          end;
      52..53: begin
             if current_span>=0 then begin

               if UID=52 then
                 current_span:=Max(0,current_span-1)
               else
                 current_span:=Min(9,current_span+1);

              OmniRig.Rig1.SendCustomCommand('SS05'+inttostr(current_span)+'0000;',0,'');
              OmniRig.Rig1.SendCustomCommand('SS06'+inttohex(current_mode,1)+'0000;',0,'');

             end;
          end;
      54: begin
            if current_mode>=0 then begin

               if (current_mode >=6) and (current_mode<=8) then
               begin
                prev_mode:=current_mode;
                current_mode := 10;
               end else
                begin
                 if (prev_mode >=6) and (prev_mode<=8) then
                  current_mode:=prev_mode
                  else current_mode:=7;

                end;

               OmniRig.Rig1.SendCustomCommand('SS06'+inttohex(current_mode,1)+'0000;',0,'');
               deadline_span:=0;

            end;
          end;
       70:
        begin
           OmniRig.Rig1.SendCustomCommand('AG0;',0,'');

        end;
       80:
        begin
          if freqInput.Visible then
           freqInput.Close
          else begin
           Application.BringToFront;
           freqInput.Visible:=true;
           freqInput.BringToFront;
           freqInput.EditFreq.SetFocus;
          end;

        end;
       81:
       begin
         SelectAntenna;
       end;
       82:
       begin
         relays2.Request(relays2.Previous);
       end;

  end;

end;

procedure TForm1.SelectAntenna;
var
  P: TPoint;
  f, i:integer;
begin

 f:=-1;
 for i:=0 to PopupMenu1.Items.Count-1 do begin
  if PopupMenu1.Items[i].Tag = relays2.Status then begin
   f:=i+1;
   break;
  end;
 end;

 P := Mouse.CursorPos;
 SetForegroundWindow(Self.Handle);
 while (f>0) do begin
     PostMessage(Self.Handle, WM_KEYDOWN, VK_DOWN, 0);
     dec(f);
 end;
 PopupMenu1.Popup(P.X, P.Y);
end;

procedure TForm1.NewFreqRequested(freq:Integer);
begin
            case OmniRig.Rig1.Vfo of
              PM_VFOA,
              PM_VFOAA, PM_VFOAB:
                       begin
                            OmniRig.Rig1.freqA:= freq;
                       end;
              PM_VFOB,
              PM_VFOBA, PM_VFOBB:
                       begin
                            OmniRig.Rig1.freqB:= freq;
                       end;
             end;

end;

procedure TForm1.TuneFreq(Delta:Int64);
begin

            case OmniRig.Rig1.Vfo of
              PM_VFOA,
              PM_VFOAA, PM_VFOAB:
                       begin
                            OmniRig.Rig1.freqA:= OmniRig.Rig1.freqA+Delta;
                       end;
              PM_VFOB,
              PM_VFOBA, PM_VFOBB:
                       begin
                            OmniRig.Rig1.freqB:= OmniRig.Rig1.freqB+Delta;
                       end;
             end;

end;

procedure TForm1.CustomREply(Sender: TObject; RigNumber: Integer; Command: OleVariant; Reply: OleVariant);

 function lookslikefloat(val:string):boolean;
 var i:integer;
 begin
  result:=true;
  for i:=1 to length(val) do
  begin
    case val[i] of
     '0'..'9', '.':
       begin
       end;
     else
       result:=false;
    end;
  end;



 end;

var cmd, rep:string;
    ecmd:cardinal;
    r:integer;
    dnr_on:boolean;
    tmp:string;
begin
 cmd:=OleVariantToAnsiString(Command);
 rep:=OleVariantToAnsiString(reply);

 Con('%s %s', [cmd, rep]);

   if cmd = 'SS04;' then begin // level
    tmp:=Copy(rep, 5, 5);
    if (length(tmp)>0) and lookslikefloat(tmp) then begin
      try
       current_level:=StrToFloat(tmp);
      except
      end;
    end;
   end else
   if cmd = 'SS05;' then begin  // span
    tmp:=Copy(rep, 5, 1);
    current_span:=StrToIntDef(tmp, -1);
   end else
   if cmd = 'SS06;' then begin // mode
    tmp:=Copy(rep, 5, 1);
    current_mode:=StrToIntDef('$'+tmp, -1);
   end else
   if cmd = 'PA0;' then begin // Preamp
     while que_preamp.PopFront(ecmd) do begin
       if preamp_deadline>xGetTickCount then begin
       r:=strtoint(rep[4]);


       case ecmd of
        1:
        begin
         if r < 2 then begin
          OmniRig.Rig1.SendCustomCommand('PA0'+IntToStr(r+1)+';', 0, '');
         end;
        end;

        2:
        begin
         if r > 0 then begin
           OmniRig.Rig1.SendCustomCommand('PA0'+IntToStr(r-1)+';', 0, '');
         end;
        end;
       end;
       end;
     end;


   end else if cmd = 'NR0;' then begin // NR
      dnr_on:=rep[4]='1';

      while que_dnr.PopFront(ecmd) do
      begin
        if dnr_deadline > xGetTickCount then begin

            case ecmd of
             1:
             begin
                   if dnr_on then
                          OmniRig.Rig1.SendCustomCommand('NR00;', 0, '')
                   else
                          OmniRig.Rig1.SendCustomCommand('NR01;', 0, '');

             end;
             2:
             begin
                   if not dnr_on then OmniRig.Rig1.SendCustomCommand('NR01;', 0, '');
             end;
             3:
             begin
                   if dnr_on then OmniRig.Rig1.SendCustomCommand('NR00;', 0, '');
                   OmniRig.Rig1.SendCustomCommand('BC00;', 0, '');
             end;


            end;


        end;
      end;


   end else if cmd = 'BC0;' then begin // DNF
      dnr_on:=rep[4]='1';
      if dnr_on then
         OmniRig.Rig1.SendCustomCommand('BC00;', 0, '')
      else
         OmniRig.Rig1.SendCustomCommand('BC01;', 0, '');

   end else if cmd = 'AG0;' then begin // Mute/unmute
     tmp:=Copy(rep, 4, 3);
     if (tmp = '000') then
       OmniRig.Rig1.SendCustomCommand('AG0'+prev_volume+';', 0, '')
     else begin
       prev_volume:=tmp;
       if (prev_volume='000') then prev_volume := '005';
       OmniRig.Rig1.SendCustomCommand('AG0000;', 0, '');
     end;

   end;

end;

procedure TForm1.MiscTimer(Sender: TObject);
begin
 Lookup;
end;

procedure TForm1.Panel1Click(Sender: TObject);
var val:string;
begin
  if InputQuery('Comamnd to send to rig', '',val) then
            OmniRig.Rig1.SendCustomCommand(val, 0, '');
end;

procedure TForm1.N11Click(Sender: TObject);
begin
 if Sender is TMenuItem then begin
  RelAltStatus:=(Sender as TMenuItem).Tag;
  relays2.Request(RelAltStatus);
 end;
end;

end.


