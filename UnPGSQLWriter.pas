unit UnPGSQLWriter;

interface

uses UnBaseRDBMSWriter, UnSourceDatabase, ADODB, ffllbase, Classes;

type
  TPGSQLWriter = class(TBaseRDBMSWriter)
  protected
    { to get sql strings }
    function GetSQL_TableDeleteIfExists(ADBTable: TDBTable): string; override;
    function GetSQL_IndexDeleteIfExists(ADBIndex: TDBIndex): string; override;

    {}
    procedure SaveStreamToParameter(APar: TParameter; AFieldType: TffFieldType; AStream: TMemoryStream); override;

  public
    constructor Create; override;

    class function GetTypeName: string; override; { short name }
    class function GetTypeFullName: string; override; { full name }
    class function GetCS_Flags: Integer; override;
    { to work with types }
    function GetDataTypeName(ADBField: TDBField): string; override;
    {}
    function GetConnectionString(const ACS_Rec: TCS_Rec): string; override;
  end;

implementation

uses SysUtils, DB;

{ TPGSQLWriter }

constructor TPGSQLWriter.Create;
begin
  inherited;
  FUseLowerCase := True;
end;

function TPGSQLWriter.GetConnectionString(const ACS_Rec: TCS_Rec): string;
begin
  Result := Format('Driver={PostgreSQL ODBC Driver(ANSI)};Server=%s;Port=%s;Database=%s;Uid=%s;Pwd=%s;',
    [ACS_Rec.Host, ACS_Rec.Port, ACS_Rec.DB, ACS_Rec.UserName, ACS_Rec.Password])
end;

class function TPGSQLWriter.GetCS_Flags: Integer;
begin
  Result := CS_Host or CS_Port or CS_DB or CS_UserName or CS_Password;
end;

function TPGSQLWriter.GetDataTypeName(ADBField: TDBField): string;
begin
  case ADBField.Type_ of
    fftBoolean: Result := 'boolean';
    fftChar, fftWideChar: Result := 'char';
    fftByte, fftInt8: Result := 'smallint'; 
    fftInt16: Result := 'smallint';
    fftWord16, fftInt32: Result := 'int';
    fftWord32, fftComp: Result := 'bigint';
    fftSingle: Result := 'real';
    fftDouble: Result := 'double precision';
    fftCurrency: Result := 'money';
    fftStDate: Result := 'date';
    fftStTime: Result := 'time';
    fftDateTime: Result := 'timestamp';
    fftExtended: Result := 'double precision';
    fftBLOBMemo,
      fftBLOBFmtMemo: Result := 'text';
    fftBLOB,
      fftBLOBOLEObj,
      fftBLOBGraphic,
      fftBLOBDBSOLEObj,
      fftBLOBTypedBin,
      fftBLOBFile,
      fftByteArray: Result := 'bytea';
    fftShortString,
      fftShortAnsiStr,
      fftNullString,
      fftNullAnsiStr,
      fftWideString: Result := Format('varchar(%d)', [ADBField.Units]);
    fftAutoInc: Result := 'serial';
  else
    Result := 'Unknown type';
  end;
end;

function TPGSQLWriter.GetSQL_IndexDeleteIfExists(ADBIndex: TDBIndex): string;
begin
  Result := Format('drop index if exists "%s";', [ADBIndex.Name]);
end;

function TPGSQLWriter.GetSQL_TableDeleteIfExists(ADBTable: TDBTable): string;
begin
  Result := Format('drop table if exists "%s";', [ADBTable.Name]);
end;

class function TPGSQLWriter.GetTypeFullName: string;
begin
  Result := 'PostgreSQL Server'
end;

class function TPGSQLWriter.GetTypeName: string;
begin
  Result := 'PG SQL'
end;

procedure TPGSQLWriter.SaveStreamToParameter(APar: TParameter;
  AFieldType: TffFieldType; AStream: TMemoryStream);
begin
  if AFieldType in [fftBLOBMemo, fftBLOBFmtMemo] then
    APar.LoadFromStream(AStream, ftMemo)
  else
    APar.LoadFromStream(AStream, ftBytes);
end;

end.

