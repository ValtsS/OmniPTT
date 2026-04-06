unit frb;

interface

uses uheartbeat, uCiaComport, uconsole, WTime, uMutex, sysutils, uQueue;


type TRelays2 = class(THThread)
 protected
  lock:tmutex;
  com:TCiaComPort;
  q:TQueue;
  buffer:string;
  Procedure Execute; override;
  Procedure ReleaseMem; override;
  Procedure Manage;
  procedure DataAvail(sender:TObject);
 public
  Status:integer;
  Constructor Create(port:integer);
  Destructor Destroy;override;
  procedure Request(mode:byte);
end;

implementation


Constructor TRelays2.Create(port:integer);
begin
 inherited Create(true);
  com:=TCiaComPort.Create(nil);
  com.Port:=port;
  com.Baudrate:=115200;
  com.OnDataAvailable:=DataAvail;
  lock:=TMutex.Create('relays2', _10sec);
  q:=TQueue.Create;
  Resume;
end;

Destructor TRelays2.Destroy;
begin
 inherited;
end;

Procedure TRelays2.ReleaseMem; // Free stuff allocated @ constructor
begin
  FreeAndNil(q);
  FreeAndNil(com);
  FreeAndNil(lock);
 inherited;
end;

Procedure TRelays2.Execute;
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

procedure TRelays2.DataAvail(sender:TObject);
var i, v:integer;
    data:string;
begin
 buffer:=buffer+com.ReceiveStr;

 for i:=1 to Length(buffer) do begin
  if (buffer[i]= ';') then begin
   data:=Copy(buffer, 1, i-1);
   delete(buffer, 1, i);
   v:=StrToIntDef(Trim(data), -1);

   if (v>=0) then self.status:=v;

  end;
 end;


end;

procedure TRelays2.Manage;
var x:cardinal;
begin
  lock.Enter;
  try
  if not com.Open then com.Open:=true;

   if com.open and q.PopFront(x) then begin
    com.SendStr(IntToStr(x)+';');
   end else if com.open then
        com.SendStr('?;');

  finally
   lock.Leave;
  end;


end;

procedure TRelays2.Request(mode:byte);
begin
 lock.Enter;
 try
  q.PushBack(mode);
 finally
  lock.Leave;
 end;
end;

end.
