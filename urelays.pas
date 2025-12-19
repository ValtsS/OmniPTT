unit urelays;

interface

uses uheartbeat, USBRelayDevice, uconsole, WTime, uMutex, sysutils, uQueue;


type TRelays = class(THThread)
 protected
  FSerial:string;
  FNr:integer;
  FHandle:TUSBRelayHandle;
  lock:tmutex;
  que:TQueue;
  Procedure Execute; override;
  Procedure ReleaseMem; override;
  Procedure Manage;
 public
  Status:integer;
  RelayState: Cardinal;
  Constructor Create(relay_serial:string; relaynr:integer);
  Destructor Destroy;override;
  Procedure RequestRelayState(channel:integer; state:boolean);
end;

implementation

Constructor TRelays.Create(relay_serial:string; relaynr:integer);
begin
 inherited Create(true);

  if usb_relay_init<>0 then
  begin
   Status:=-2;
   Con('USB relay init failed');
  end else begin
   Status:=-1;
   Con('USB relay init OK');
  end;
  FSerial:=relay_serial;
  FNr:=relaynr;
  lock:=TMutex.Create('_relaylock', 1000);
  que:=TQueue.Create;
  Resume;
end;

Destructor TRelays.Destroy;
begin
 inherited;
end;

Procedure TRelays.ReleaseMem; // Free stuff allocated @ constructor
begin
  lock.Enter();
   FreeAndNil(que);
  try
  if FHandle<>0 then begin
   usb_relay_device_close(Fhandle);
   Fhandle:=0;
  end;
  if status>-2 then
   usb_relay_exit;
   finally
    FreeAndNil(lock);
   end;
 inherited;
end;

Procedure TRelays.Execute;
begin
  StartTickler;
  try
   HeartBeat(self);
    while MessageLoop do begin
     HeartBeat(self);
      Manage;
    end;
  finally
   StopTickler;
  end;
end;

procedure TRelays.Manage;
var channel, state:cardinal;
begin
  if FHandle = 0 then
   FHandle:=usb_relay_device_open_with_serial_number(PChar(FSerial), length(FSerial));

  if (fhandle<>0) then begin

   lock.Enter;
   try
     if (que.PopFront(channel)) then begin
       que.PopFront(state);

       if boolean(state) then
          usb_relay_device_open_one_relay_channel(fhandle, channel)
       else
          usb_relay_device_close_one_relay_channel(fhandle, channel);

     end;
   finally
    lock.Leave;
   end;


   status:=usb_relay_device_get_status(FHandle, relaystate);

  end else status:=-1;


 Con('%x %x',[status, relaystate]);

end;

Procedure TRelays.RequestRelayState(channel:integer; state:boolean);
begin
 lock.Enter;
 try
  que.PushBack(channel);
  que.PushBack(cardinal(state));
 finally
  lock.Leave;
 end;
 Tickle;
end;


end.
