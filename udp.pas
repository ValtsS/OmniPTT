unit udp;

interface

uses
  Windows, 
  Winsock,
  SysUtils;

// Custom type definition for a dynamic array of Byte (replaces TBytes in D5)
type
  TByteArrayD5 = array of Byte;

/// <summary>
/// Sends a UDP datagram containing a UTF-8 encoded XML structure.
/// </summary>
/// <param name="frequency">The frequency value to embed in the XML.</param>
/// <param name="host">The destination IP address (e.g., '127.0.0.1').</param>
/// <param name="port">The destination port number.</param>
procedure SendUDPInfo(frequency: Int64; host: string; port: word);

implementation

// --- Type and Constant Definitions ---

// Winsock error checking helper
procedure CheckWinsockError(const FuncName: string; ResultCode: Integer);
begin
  if ResultCode = SOCKET_ERROR then
  begin
    raise Exception.CreateFmt('Winsock error in %s. Error Code: %d', [FuncName, WSAGetLastError]);
  end;
end;

// --- XML Generation Function ---

/// <summary>
/// Generates the XML string, converts it to a UTF-8 byte array (TByteArrayD5), 
/// and returns the buffer. The caller is responsible for the buffer's lifecycle 
/// (i.e., it will be implicitly managed by the compiler upon exit).
/// </summary>
/// <param name="frequency">The frequency value to embed.</param>
/// <returns>A dynamic array of Byte containing the UTF-8 encoded XML.</returns>
function GenerateUTF8Datagram(frequency: Int64): TByteArrayD5;
var
  XMLDatagram: string;
  UTF8Buffer: TByteArrayD5;
  UTF8Length: Integer;
  ResultCode: Integer;
begin
  // 1. Construct the XML datagram (using UTF-8 encoding declaration)
  XMLDatagram := 
    '<?xml version="1.0" encoding="utf-8" standalone="yes"?>' + #13#10 +
    '<RadioInfo>' + #13#10 +
    '  <RadioNr>1</RadioNr>' + #13#10 +
    '  <TXFreq>' + IntToStr(frequency) + '</TXFreq>' + #13#10 +
    '  <FocusRadioNr>1</FocusRadioNr>' + #13#10 +
    '</RadioInfo>';

  // 2. Convert the AnsiString XML to a UTF-8 Byte Buffer
  
  // A. Get the required size for the UTF-8 buffer.
  // We use CP_UTF8 (65001) as the code page. 
  // We must convert the AnsiString to WideString temporarily for WideCharToMultiByte.
  UTF8Length := WideCharToMultiByte(
    CP_UTF8,                    
    0,                          
    PWideChar(WideString(XMLDatagram)), 
    Length(XMLDatagram),        
    nil,                        
    0,                          
    nil,                        
    nil                         
  );

  if UTF8Length = 0 then
  begin
    raise Exception.Create('Failed to calculate required UTF-8 buffer size.');
  end;

  // B. Allocate the dynamic array buffer
  SetLength(UTF8Buffer, UTF8Length);
  
  // C. Perform the actual conversion to the UTF-8 buffer
  ResultCode := WideCharToMultiByte(
    CP_UTF8,
    0,
    PWideChar(WideString(XMLDatagram)),
    Length(XMLDatagram),
    PAnsiChar(UTF8Buffer),      // Pointer to the first byte of the array
    UTF8Length,
    nil,
    nil
  );

  if ResultCode = 0 then
  begin
    raise Exception.Create('Failed to convert XML to UTF-8.');
  end;

  // D. Return the buffer
  Result := UTF8Buffer;
end;

// --- Main Sending Procedure ---

procedure SendUDPInfo(frequency: Int64; host: string; port: word);
var
  WSAData: TWSAData;
  UDPSocket: TSocket;
  TargetAddr: TSockAddrIn;
  UTF8Buffer: TByteArrayD5;
  BytesSent: Integer;
  ResultCode: Integer;
begin
  // 1. Initialize Winsock
  ResultCode := WSAStartup($0202, WSAData);
  CheckWinsockError('WSAStartup', ResultCode);

  try
    // 2. Create a UDP Socket
    UDPSocket := socket(AF_INET, SOCK_DGRAM, IPPROTO_IP);
    if UDPSocket = INVALID_SOCKET then
    begin
      CheckWinsockError('socket', SOCKET_ERROR);
    end;

    try
      // 3. Prepare the destination address structure
      FillChar(TargetAddr, SizeOf(TargetAddr), 0);
      TargetAddr.sin_family := AF_INET;
      TargetAddr.sin_port := htons(port);
      // NOTE: host must be a dotted-decimal IP string (e.g., '192.168.1.1')
      TargetAddr.sin_addr.S_addr := inet_addr(PAnsiChar(host)); 
      
      if TargetAddr.sin_addr.S_addr = INADDR_NONE then
      begin
        raise Exception.CreateFmt('Invalid host address: %s', [host]);
      end;

      // 4. Generate the UTF-8 Datagram in the separate function
      UTF8Buffer := GenerateUTF8Datagram(frequency);

      // 5. Send the UDP datagram
      BytesSent := sendto(
        UDPSocket,
        PAnsiChar(UTF8Buffer)^,         // Pointer to the raw UTF-8 byte data
        Length(UTF8Buffer),             // Length of the data in bytes
        0,
        TargetAddr,
        SizeOf(TargetAddr)
      );

      if BytesSent < 0 then
      begin
        CheckWinsockError('sendto', SOCKET_ERROR);
      end;
      
    finally
      // 6. Close the socket
      closesocket(UDPSocket);
    end;

  finally
    // 7. Clean up Winsock
    WSACleanup;
  end;
end;

initialization

finalization

end.
