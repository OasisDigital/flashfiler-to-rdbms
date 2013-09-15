program FF_RDBMS_Converter;

uses
  ShareMem,
  SysUtils,
  Forms,
  UnMain in 'UnMain.pas' {fmFF_RDBMS_Converter},
  UndmBaseConn in 'UndmBaseConn.pas' {dmBaseConn: TDataModule},
  UnSourceDatabase in 'UnSourceDatabase.pas',
  UnSourceDBLoader in 'UnSourceDBLoader.pas',
  UnLog in 'UnLog.pas',
  UnConverterSettings in 'UnConverterSettings.pas',
  UnProcAll in 'UnProcAll.pas',
  UnBaseRDBMSWriter in 'UnBaseRDBMSWriter.pas',
  UnMSSQLWriter in 'UnMSSQLWriter.pas',
  UnPGSQLWriter in 'UnPGSQLWriter.pas';

{$R *.res}

begin
  Application.Initialize;
  Log := TLog.Create;
  Application.CreateForm(TfmFF_RDBMS_Converter, fmFF_RDBMS_Converter);
  Application.Run;
  FreeAndNil(Log);
end.
