{*******************************************************}
{                                                       }
{       Variant Utilities                               }
{       Delphi 5 Compatible                             }
{                                                       }
{       Provides conversion functions for OleVariant    }
{       byte arrays to AnsiString                       }
{                                                       }
{*******************************************************}

unit VariantUtils;

interface

uses
  SysUtils, ActiveX;

{ Converts an OleVariant containing a byte array to an AnsiString.
  The variant must be a one-dimensional array of bytes (varByte).
  
  Parameters:
    V - The OleVariant containing the byte array
    
  Returns:
    AnsiString containing the bytes from the array
    
  Raises:
    EVariantError if V is not a byte array or is not an array at all }
function OleVariantToAnsiString(const V: OleVariant): AnsiString;

{ Checks if an OleVariant is a byte array }
function IsVariantByteArray(const V: OleVariant): Boolean;

implementation

function IsVariantByteArray(const V: OleVariant): Boolean;
var
  VType: TVarType;
begin
  VType := VarType(V);
  Result := (VType and varTypeMask = varByte) and 
            (VType and varArray = varArray);
end;

function OleVariantToAnsiString(const V: OleVariant): AnsiString;
var
  LowBound, HighBound: Integer;
  DataPtr: Pointer;
  DataLen: Integer;
begin
  Result := '';
  
  // Check if the variant is null or empty
  if VarIsNull(V) or VarIsEmpty(V) then
    Exit;
  
  // Verify that V is a byte array
  if not IsVariantByteArray(V) then
    raise EVariantError.Create('OleVariantToAnsiString: Variant is not a byte array');
  
  // Verify it's a one-dimensional array
  if VarArrayDimCount(V) <> 1 then
    raise EVariantError.Create('OleVariantToAnsiString: Variant must be a one-dimensional array');
  
  // Get array bounds
  LowBound := VarArrayLowBound(V, 1);
  HighBound := VarArrayHighBound(V, 1);
  
  // Calculate length
  DataLen := HighBound - LowBound + 1;
  
  if DataLen <= 0 then
    Exit;
  
  // Allocate the result string
  SetLength(Result, DataLen);
  
  // Lock the array to get a pointer to the data
  DataPtr := VarArrayLock(V);
  try
    // Copy the bytes to the string
    Move(DataPtr^, Result[1], DataLen);
  finally
    // Always unlock the array
    VarArrayUnlock(V);
  end;
end;




end.

