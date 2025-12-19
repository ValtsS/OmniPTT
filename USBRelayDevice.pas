unit USBRelayDevice;

interface

uses
  Windows, SysUtils;

const
  // Library version
  USBRELAY_LIB_VER = $02;

  // Device types
  USB_RELAY_DEVICE_ONE_CHANNEL   = 1;
  USB_RELAY_DEVICE_TWO_CHANNEL   = 2;
  USB_RELAY_DEVICE_FOUR_CHANNEL  = 4;
  USB_RELAY_DEVICE_EIGHT_CHANNEL = 8;

type
  // Handle type
  TUSBRelayHandle = Integer;

  // Forward declaration of device info structure
  PUSBRelayDeviceInfo = ^TUSBRelayDeviceInfo;
  TUSBRelayDeviceInfo = record
    SerialNumber: PAnsiChar;
    DevicePath: PAnsiChar;
    Type_: Integer;  // enum usb_relay_device_type
    Next: PUSBRelayDeviceInfo;
  end;

  PUSBRelayDeviceInfoPtr = ^PUSBRelayDeviceInfo;

// Function prototypes
function usb_relay_init: Integer; cdecl;
function usb_relay_exit: Integer; cdecl;
function usb_relay_device_enumerate: PUSBRelayDeviceInfo; cdecl;
procedure usb_relay_device_free_enumerate(DeviceInfo: PUSBRelayDeviceInfo); cdecl;
function usb_relay_device_open_with_serial_number(SerialNumber: PAnsiChar; Len: Cardinal): TUSBRelayHandle; cdecl;
function usb_relay_device_open(DeviceInfo: PUSBRelayDeviceInfo): TUSBRelayHandle; cdecl;
procedure usb_relay_device_close(Handle: TUSBRelayHandle); cdecl;
function usb_relay_device_open_one_relay_channel(Handle: TUSBRelayHandle; Index: Integer): Integer; cdecl;
function usb_relay_device_open_all_relay_channel(Handle: TUSBRelayHandle): Integer; cdecl;
function usb_relay_device_close_one_relay_channel(Handle: TUSBRelayHandle; Index: Integer): Integer; cdecl;
function usb_relay_device_close_all_relay_channel(Handle: TUSBRelayHandle): Integer; cdecl;
function usb_relay_device_get_status(Handle: TUSBRelayHandle; var Status: Cardinal): Integer; cdecl;
function usb_relay_device_lib_version: Integer; cdecl;
function usb_relay_device_next_dev(DeviceInfo: Pointer): Pointer; cdecl;
function usb_relay_device_get_num_relays(DeviceInfo: Pointer): Integer; cdecl;
function usb_relay_device_get_id_string(DeviceInfo: Pointer): Pointer; cdecl;
function usb_relay_device_get_status_bitmap(Handle: TUSBRelayHandle): Integer; cdecl;

implementation

const
  USBRelayLib = 'usb_relay_device.dll';

// Load the DLL
var
  USBRelayModule: HMODULE = 0;

// Function pointers to DLL functions
function usb_relay_init: Integer; cdecl; external USBRelayLib name 'usb_relay_init';
function usb_relay_exit: Integer; cdecl; external USBRelayLib name 'usb_relay_exit';
function usb_relay_device_enumerate: PUSBRelayDeviceInfo; cdecl; external USBRelayLib name 'usb_relay_device_enumerate';
procedure usb_relay_device_free_enumerate(DeviceInfo: PUSBRelayDeviceInfo); cdecl; external USBRelayLib name 'usb_relay_device_free_enumerate';
function usb_relay_device_open_with_serial_number(SerialNumber: PAnsiChar; Len: Cardinal): TUSBRelayHandle; cdecl; external USBRelayLib name 'usb_relay_device_open_with_serial_number';
function usb_relay_device_open(DeviceInfo: PUSBRelayDeviceInfo): TUSBRelayHandle; cdecl; external USBRelayLib name 'usb_relay_device_open';
procedure usb_relay_device_close(Handle: TUSBRelayHandle); cdecl; external USBRelayLib name 'usb_relay_device_close';
function usb_relay_device_open_one_relay_channel(Handle: TUSBRelayHandle; Index: Integer): Integer; cdecl; external USBRelayLib name 'usb_relay_device_open_one_relay_channel';
function usb_relay_device_open_all_relay_channel(Handle: TUSBRelayHandle): Integer; cdecl; external USBRelayLib name 'usb_relay_device_open_all_relay_channel';
function usb_relay_device_close_one_relay_channel(Handle: TUSBRelayHandle; Index: Integer): Integer; cdecl; external USBRelayLib name 'usb_relay_device_close_one_relay_channel';
function usb_relay_device_close_all_relay_channel(Handle: TUSBRelayHandle): Integer; cdecl; external USBRelayLib name 'usb_relay_device_close_all_relay_channel';
function usb_relay_device_get_status(Handle: TUSBRelayHandle; var Status: Cardinal): Integer; cdecl; external USBRelayLib name 'usb_relay_device_get_status';
function usb_relay_device_lib_version: Integer; cdecl; external USBRelayLib name 'usb_relay_device_lib_version';
function usb_relay_device_next_dev(DeviceInfo: Pointer): Pointer; cdecl; external USBRelayLib name 'usb_relay_device_next_dev';
function usb_relay_device_get_num_relays(DeviceInfo: Pointer): Integer; cdecl; external USBRelayLib name 'usb_relay_device_get_num_relays';
function usb_relay_device_get_id_string(DeviceInfo: Pointer): Pointer; cdecl; external USBRelayLib name 'usb_relay_device_get_id_string';
function usb_relay_device_get_status_bitmap(Handle: TUSBRelayHandle): Integer; cdecl; external USBRelayLib name 'usb_relay_device_get_status_bitmap';

initialization
  // Load the DLL
  USBRelayModule := LoadLibrary(PChar(USBRelayLib));
  if USBRelayModule = 0 then
    raise Exception.Create('Failed to load USB relay device DLL: ' + USBRelayLib);

finalization
  // Unload the DLL
  if USBRelayModule <> 0 then
    FreeLibrary(USBRelayModule);
end.
