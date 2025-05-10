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
  Registry;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Exit1Click(Sender: TObject);
  private
    procedure StatusChangeEvent(Sender: TObject; RigNumber: Integer);
    procedure ParamsChangeEvent(Sender: TObject; RigNumber, Params: Integer);
    procedure RigTypeChangeEvent(Sender: TObject; RigNumber: Integer);

    procedure WmKeyboard(var Msg: TMessage); message WM_HOOK_KEY;
    Function  IsPTTActive:boolean;
    procedure CreateRigControl;
    procedure KillRigControl;

    procedure AdjustDown;
  protected
    reg:TRegistry;
    procedure SavePos;
  public
    OmniRig: TOmniRigX;
    PTTUnti:Int64;
    PTTDown:boolean;
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

//------------------------------------------------------------------------------
//                  OmniRig object creation and destruction
//------------------------------------------------------------------------------
procedure TForm1.FormCreate(Sender: TObject);
var l,t:integer;
begin
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

  CreateRigControl;
  StartKeyboardHook(WindowHandle);
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
begin
  if OmniRig = nil then Exit;

  case RigNumber of
    1:
      if OmniRig.Rig1.Status = ST_ONLINE
        then
          //display frequency and mode
          begin
              Timer1.Enabled:=true;
          end
        else
          //if rig is not online, clear the fields
          begin
              Timer1.Enabled:=false;
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
    1: Panel1.Caption := OmniRig.Rig1.StatusStr;
    end;
end;


Function TForm1.IsPTTActive:boolean;
begin
 result:=OmniRig.Rig1.Tx = PM_TX;
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
         OmniRig.Rig1.Tx := PM_TX;
         DidIEnable:=true;
    end
    else if DidIEnable then begin
         OmniRig.Rig1.Tx := PM_RX;
         DidIEnable:=false;
    end;
  end;

  if (IsPTTActive) then
  begin
    Panel1.Color:=clRed;
  end else begin
    Panel1.Color:=clBtnFace;
  end;


end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
 if IsPTTActive then
  OmniRig.Rig1.Tx := PM_RX;

 StopKeyboardHook;
 Timer1.Enabled:=false;
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
 CanClose:=false;
 SavePos;
 WindowState:=wsMinimized;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
 SavePos;
 Close;
end;

end.

