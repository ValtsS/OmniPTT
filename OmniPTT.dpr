program OmniPTT;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  OmniRig_TLB in 'OmniRig_TLB.pas',
  uhook in 'uhook.pas',
  udp in 'udp.pas',
  VariantUtils in 'VariantUtils.pas',
  IniReader in 'IniReader.pas',
  finputfreq in 'finputfreq.pas' {FreqInputForm},
  frb in 'frb.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

