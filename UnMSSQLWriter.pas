unit UnMSSQLWriter;

interface

uses UnBaseRDBMSWriter, UnSourceDatabase;

type
  TMSSQLWriter = class(TBaseRDBMSWriter)
  private
    { getting only a type without additional information such as identity(1,1) }
    function GetDataTypeShortName(ADBField: TDBField): string;
  protected
    function GetCanUseNonUnicodeTypes: Boolean; override;
    function GetCanUseIdentity: Boolean; override;
    { to get sql strings }
    function GetSQL_TableDeleteIfExists(ADBTable: TDBTable): string; override;
    function GetSQL_IndexDeleteIfExists(ADBIndex: TDBIndex): string; override;
    procedure GetSQL_TableAddPrimaryKey(ADBTable: TDBTable; var ASQL_SetNotNull, ASQL_AddPrimaryKey: string); override;
    function GetScript_EndOfBatch: string; override;
    { to enable\disable some controls, such as to edit an identity }
    procedure BeginUpdate(ADBTable: TDBTable); override;
    procedure EndUpdate(ADBTable: TDBTable); override;
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

uses SysUtils, UnLog, ffllbase, StrUtils, ADODB, DB;

{ TMSSQLWriter }

procedure TMSSQLWriter.BeginUpdate(ADBTable: TDBTable);
var
  i: Integer;
  f: Boolean;
begin
  if not UseIdentity then Exit;

  f := False;
  for i := 0 to ADBTable.Count - 1 do
    if ADBTable[i].Type_ = fftAutoInc then begin
      f := True;
      Break;
    end;

  if f then
    try
      CmdCreate.CommandText := Format('set identity_insert "%s" on;', [ADBTable.Name]);
      CmdCreate.Execute;
    except
      on e: Exception do begin
        Log.Add(ekSQLCreateFailed, 0, CmdCreate.CommandText);
        Log.Add(lkError, e.Message);
      end;
    end;
end;

procedure TMSSQLWriter.EndUpdate(ADBTable: TDBTable);
var
  i: Integer;
  f: Boolean;
begin
  if not UseIdentity then Exit;

  f := False;
  for i := 0 to ADBTable.Count - 1 do
    if ADBTable[i].Type_ = fftAutoInc then begin
      f := True;
      Break;
    end;

  if f then
    try
      CmdCreate.CommandText := Format('set identity_insert "%s" off;', [ADBTable.Name]);
      CmdCreate.Execute;
    except
      on e: Exception do begin
        Log.Add(ekSQLCreateFailed, 0, CmdCreate.CommandText);
        Log.Add(lkError, e.Message);
      end;
    end;
end;

function TMSSQLWriter.GetCanUseNonUnicodeTypes: Boolean;
begin
  Result := True;
end;

function TMSSQLWriter.GetConnectionString(const ACS_Rec: TCS_Rec): string;
begin
  if not ACS_Rec.Trusted then
    Result := Format('Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s',
      [ACS_Rec.Host, ACS_Rec.DB, ACS_Rec.UserName, ACS_Rec.Password])
  else
    Result := Format('Driver={SQL Server};Server=%s;Database=%s;Trusted_Connection=yes;',
      [ACS_Rec.Host, ACS_Rec.DB]);
end;

class function TMSSQLWriter.GetCS_Flags: Integer;
begin
  Result := CS_Host or CS_DB or CS_UserName or CS_Password or CS_Trusted;
end;

function TMSSQLWriter.GetDataTypeShortName(ADBField: TDBField): string;
begin
  case ADBField.Type_ of
    fftBoolean: Result := 'bit';
    fftChar, fftWideChar: Result := 'nchar';
    fftByte, fftInt8: Result := 'smallint'; 
    fftInt16: Result := 'smallint';
    fftWord16, fftInt32: Result := 'int';
    fftWord32, fftComp: Result := 'bigint';
    fftSingle: Result := 'real';
    fftDouble: Result := 'double precision';
    fftCurrency: Result := 'money';
    fftStDate: Result := 'date';
    fftStTime: Result := 'time';
    fftDateTime: Result := 'datetime';
    fftExtended: Result := 'double precision';
    fftBLOBMemo,
      fftBLOBFmtMemo: Result := IfThen(CanUseNonUnicodeTypes and UseNonUnicodeTypes, 'text', 'ntext');
    fftBLOB,
      fftBLOBOLEObj,
      fftBLOBGraphic,
      fftBLOBDBSOLEObj,
      fftBLOBTypedBin,
      fftBLOBFile,
      fftByteArray: Result := 'varbinary(max)';
    fftShortString,
      fftShortAnsiStr,
      fftNullString,
      fftNullAnsiStr,
      fftWideString: Result := Format(IfThen(CanUseNonUnicodeTypes and UseNonUnicodeTypes, 'varchar(%d)', 'nvarchar(%d)'), [ADBField.Units]);
    fftAutoInc: Result := 'int';
  else
    Result := 'Unknown type';
  end;  
end;

function TMSSQLWriter.GetDataTypeName(ADBField: TDBField): string;
begin
  Result := GetDataTypeShortName(ADBField);
  if (ADBField.Type_ = fftAutoInc)  and UseIdentity then
    Result := Result + ' identity(1,1)';
end;

function TMSSQLWriter.GetSQL_IndexDeleteIfExists(ADBIndex: TDBIndex): string;
begin
  Result := Format('if object_id(''%0:s'', ''U'') is not null drop index "%0:s" on "%1:s";', [ADBIndex.Name, ADBIndex.Indexes.Table.Name])
end;

procedure TMSSQLWriter.GetSQL_TableAddPrimaryKey(ADBTable: TDBTable; var ASQL_SetNotNull, ASQL_AddPrimaryKey: string);
var
  i: Integer;
begin
  inherited GetSQL_TableAddPrimaryKey(ADBTable, ASQL_SetNotNull, ASQL_AddPrimaryKey);

  ASQL_SetNotNull := '';
  if ADBTable.PrimaryKeyCount > 0 then
    for i := 0 to ADBTable.PrimaryKeyCount - 1 do
      ASQL_SetNotNull := ASQL_SetNotNull + Format('alter table "%s" alter column "%s" %s not null;',
        [ADBTable.Name, ADBTable.PrimaryKey[i].Name, GetDataTypeShortName(ADBTable.PrimaryKey[i])]) + #13#10;  
end;

function TMSSQLWriter.GetSQL_TableDeleteIfExists(ADBTable: TDBTable): string;
begin
  Result := Format('if object_id(''%0:s'', ''U'') is not null  drop table "%0:s";', [ADBTable.Name])
end;

class function TMSSQLWriter.GetTypeFullName: string;
begin
  Result := 'Microsoft SQL Server'
end;

class function TMSSQLWriter.GetTypeName: string;
begin
  Result := 'MS SQL'
end;

function TMSSQLWriter.GetScript_EndOfBatch: string;
begin
  Result := 'GO';
end;

constructor TMSSQLWriter.Create;
begin
  inherited;
  FUseLowerCase := False;
end;

function TMSSQLWriter.GetCanUseIdentity: Boolean;
begin
  Result := True;
end;

end.

