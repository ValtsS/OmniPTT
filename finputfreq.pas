unit finputfreq;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, UConsole;

type
   TOnFreqRequest = procedure (NewFreq:integer) of object;

type
  TFreqInputForm = class(TForm)
    EditFreq: TEdit;
    procedure EditFreqKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    FOnFreqRequest:TOnFreqRequest;
  public
    { Public declarations }
     property OnFreqChange:TOnFreqRequest read FOnFreqRequest write FOnFreqRequest;
  end;

implementation

{$R *.DFM}

function ForceStrToFloat(const S: string): Extended;
var
  OldSeparator: Char;
begin
  OldSeparator := DecimalSeparator;
  try
    DecimalSeparator := '.';
    Result := StrToFloat(S);
  finally
    DecimalSeparator := OldSeparator;
  end;
end;



procedure TFreqInputForm.EditFreqKeyPress(Sender: TObject; var Key: Char);
var t:string;
    f:Integer;
    ef:extended;
begin
 f:=0;
 if key=#27 then begin
  close;
 end else
 if key=#13 then begin
   f:=0;
   t:=trim((Sender as TEdit).Text);

   if (pos('.', t) > 0) then begin

     ef:=ForceStrToFloat(t);

     if ef<72 then ef:=ef*1e6;
     if ef<72e3 then ef:=ef*1e3;

     f:=round(ef);


   end else begin
     f:=StrToIntDef(t, 0);

     if (f < 72) then f:=f*integer(1000000);
      if (f< 72000) then f:=f*1000;

   end
  end else if not (Key in ['0'..'9', '.', #8]) then
    Key := #0
  else if (Key = '.') and (Pos('.', TEdit(Sender).Text) > 0) then
    Key := #0;

  if (f>0) and assigned(FOnFreqRequest) then begin
   TEdit(Sender).text:='';
   FOnFreqRequest(f);
   Close;
  end;

end;

end.
