unit IniReader;

interface

uses
  Windows, SysUtils, Classes;

type
  // Exception class for INI reading errors
  EIniReaderException = class(Exception);

  // Main INI reader class
  TIniReader = class
  private
    FFileName: string;
    FSection: string;
  public
    constructor Create(const ASection: string = 'DEFAULT');
    constructor CreateFromFile(const AFileName: string; const ASection: string = 'DEFAULT');
    destructor Destroy; override;
    
    // String reading methods
    function ReadString(const Key: string; const Default: string = ''): string;
    procedure WriteString(const Key, Value: string);
    
    // Integer reading methods
    function ReadInteger(const Key: string; const Default: Integer = 0): Integer;
    procedure WriteInteger(const Key: string; const Value: Integer);
    
    // Boolean reading methods
    function ReadBool(const Key: string; const Default: Boolean = False): Boolean;
    procedure WriteBool(const Key: string; const Value: Boolean);
    
    // Float reading methods
    function ReadFloat(const Key: string; const Default: Double = 0.0): Double;
    procedure WriteFloat(const Key: string; const Value: Double);
    
    // Property accessors
    property FileName: string read FFileName;
    property Section: string read FSection write FSection;
  end;

// Helper functions for quick access
function IniReadString(const Section, Key: string; const Default: string = ''): string;
function IniReadInteger(const Section, Key: string; const Default: Integer = 0): Integer;
function IniReadBool(const Section, Key: string; const Default: Boolean = False): Boolean;
function IniReadFloat(const Section, Key: string; const Default: Double = 0.0): Double;

// Helper functions for Boolean conversion (needed for Delphi 5)
function BoolToStr(Value: Boolean; UseBoolStrs: Boolean): string;
function StrToBool(const S: string): Boolean;

implementation

{ TIniReader }

function GetDefaultIniFileName: string;
var
  ExeName: string;
  I: Integer;
begin
  // Get the executable name
  SetLength(ExeName, MAX_PATH);
  GetModuleFileName(0, PChar(ExeName), MAX_PATH);
  ExeName := PChar(ExeName);
  
  // Remove the .exe extension
  I := LastDelimiter('.', ExeName);
  if I > 0 then
    SetLength(ExeName, I - 1);
  
  // Add .ini extension
  Result := ExeName + '.ini';
end;

constructor TIniReader.Create(const ASection: string = 'DEFAULT');
begin
  inherited Create;
  FFileName := GetDefaultIniFileName;
  FSection := ASection;
end;

constructor TIniReader.CreateFromFile(const AFileName: string; const ASection: string = 'DEFAULT');
begin
  inherited Create;
  FFileName := AFileName;
  FSection := ASection;
end;

destructor TIniReader.Destroy;
begin
  inherited Destroy;
end;

function TIniReader.ReadString(const Key: string; const Default: string = ''): string;
begin
  SetLength(Result, 512);
  GetPrivateProfileString(PChar(FSection), PChar(Key), PChar(Default), PChar(Result), 
                          Length(Result), PChar(FFileName));
end;

procedure TIniReader.WriteString(const Key, Value: string);
begin
  if not WritePrivateProfileString(PChar(FSection), PChar(Key), PChar(Value), PChar(FFileName)) then
    raise EIniReaderException.Create('Failed to write string to INI file');
end;

function TIniReader.ReadInteger(const Key: string; const Default: Integer = 0): Integer;
begin
  Result := GetPrivateProfileInt(PChar(FSection), PChar(Key), Default, PChar(FFileName));
end;

procedure TIniReader.WriteInteger(const Key: string; const Value: Integer);
begin
  WriteString(Key, IntToStr(Value));
end;

function TIniReader.ReadBool(const Key: string; const Default: Boolean = False): Boolean;
var
  Value: string;
begin
  // Convert boolean to string for reading
  Value := ReadString(Key, BoolToStr(Default, True));
  Result := StrToBool(Value);
end;

procedure TIniReader.WriteBool(const Key: string; const Value: Boolean);
begin
  WriteString(Key, BoolToStr(Value, True));
end;

function TIniReader.ReadFloat(const Key: string; const Default: Double = 0.0): Double;
var
  Value: string;
begin
  Value := ReadString(Key, FloatToStr(Default));
  Result := StrToFloat(Value);
end;

procedure TIniReader.WriteFloat(const Key: string; const Value: Double);
begin
  WriteString(Key, FloatToStr(Value));
end;

// Helper functions for Boolean conversion (needed for Delphi 5)
function BoolToStr(Value: Boolean; UseBoolStrs: Boolean): string;
begin
  if Value then
    Result := '1'
  else
    Result := '0';
end;

function StrToBool(const S: string): Boolean;
begin
  Result := (CompareText(S, '1') = 0) or (CompareText(S, 'TRUE') = 0) or 
            (CompareText(S, 'YES') = 0) or (CompareText(S, 'ON') = 0);
end;

{ Helper Functions }

function IniReadString(const Section, Key: string; const Default: string = ''): string;
begin
  SetLength(Result, 512);
  GetPrivateProfileString(PChar(Section), PChar(Key), PChar(Default), PChar(Result), 
                          Length(Result), PChar(GetDefaultIniFileName));
end;

function IniReadInteger(const Section, Key: string; const Default: Integer = 0): Integer;
begin
  Result := GetPrivateProfileInt(PChar(Section), PChar(Key), Default, PChar(GetDefaultIniFileName));
end;

function IniReadBool(const Section, Key: string; const Default: Boolean = False): Boolean;
var
  Value: string;
begin
  Value := IniReadString(Section, Key, BoolToStr(Default, True));
  Result := StrToBool(Value);
end;

function IniReadFloat(const Section, Key: string; const Default: Double = 0.0): Double;
var
  Value: string;
begin
  Value := IniReadString(Section, Key, FloatToStr(Default));
  Result := StrToFloat(Value);
end;

end.

