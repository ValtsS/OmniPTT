unit uhook;

interface


uses
  Windows, Messages, SysUtils;

const WM_HOOK_KEY = WM_USER + 1;

procedure StartKeyboardHook(wndhandle:cardinal);
procedure StopKeyboardHook;

Function IsDown:boolean;

implementation


const
  VK_OEM_5 = $DC; // \ key
  VK_PUSH_TO_TALK = VK_OEM_5; // \ key

const
  WH_KEYBOARD_LL = 13;
  WM_KEYDOWN     = $0100;
  WM_KEYUP       = $0101;

var
  HookHandle: HHOOK = 0;
  IsPushingToTalk: Boolean = False;
  NotifyTo:Cardinal;

Function IsDown:boolean;
begin
 result:=IsPushingToTalk;
end;

type
  ULONG_PTR = LongWord;

type
  PKBDLLHookStruct = ^KBDLLHOOKSTRUCT;
  KBDLLHOOKSTRUCT = packed record
    vkCode: DWORD;
    scanCode: DWORD;
    flags: DWORD;
    time: DWORD;
    dwExtraInfo: ULONG_PTR;
  end;


function IsModifierDown: Boolean;
begin
  Result :=
    (GetAsyncKeyState(VK_SHIFT) < 0) or
    (GetAsyncKeyState(VK_CONTROL) < 0) or
    (GetAsyncKeyState(VK_MENU) < 0); // Alt
end;

             
function LowLevelKeyboardProc(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  p: PKBDLLHookStruct;
  processNormally:boolean;
begin
  processNormally := true;
  if nCode = HC_ACTION then
  begin
    p := PKBDLLHookStruct(lParam);
    if p^.vkCode = VK_PUSH_TO_TALK then
    begin
      processNormally := IsModifierDown;
      if wParam = WM_KEYDOWN then
      begin
        if not IsPushingToTalk and not processNormally then
        begin
          IsPushingToTalk := True;
          PostMessage(NotifyTo, WM_HOOK_KEY, 1, 0);
          OutputDebugString('Push-to-talk activated');
          // Start transmitting
        end;
      end
      else if wParam = WM_KEYUP then
      begin
        IsPushingToTalk := False;
        PostMessage(NotifyTo, WM_HOOK_KEY, 0, 0);
        OutputDebugString('Push-to-talk deactivated');
        // Stop transmitting
      end;

      if not processNormally then begin
       Result := 1;
       Exit;
      end;
      
    end;
  end;
  Result := CallNextHookEx(HookHandle, nCode, wParam, lParam);
end;

procedure StartKeyboardHook(wndhandle:cardinal);
begin
  if (HookHandle<>0) then
   Raise Exception.Create('Already in use'); 
  NotifyTo := wndhandle;
  HookHandle := SetWindowsHookEx(WH_KEYBOARD_LL, @LowLevelKeyboardProc, HInstance, 0);
end;

procedure StopKeyboardHook;
begin
  if HookHandle <> 0 then
  begin
    UnhookWindowsHookEx(HookHandle);
    HookHandle := 0;
  end;
end;

end.
