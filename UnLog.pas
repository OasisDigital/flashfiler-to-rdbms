unit UnLog;

interface

uses StdCtrls, UnSourceDatabase, ADODB, UnBaseRDBMSWriter;

type
  TLogKind = (lkInfo, lkError, lkSuccess);
  TLogErrorKind = (
    ekSQLConnectFailed, { when try to connect to sql server }
    ekFFConnectFailed, { when try to connect to FF database }
    ekFFServerIsBroken, { when try to communicate with FF database }
    ekFFGetDataFailed, { when try to pull out a set of data from FF db }
    ekSQLCreateFailed, { when try to create ( or drop if exists ) a table in sql server }
    ekFFConvertBlob, { when try to convert FF blob field to SQL Server }
    ekSQLInsertFailed { when try to insert new record into SQL Server }
    );
  TLog = class
  private
    FTextFile: Textfile;
    FLogFile: string;
    FControl: TMemo;

    procedure SetLogFile(AValue: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(const AKind: TLogKind; const AMessage: string); overload;
    procedure Add(const AErrorKind: TLogErrorKind; const ACode: Integer; const AMessage: string); overload;

    procedure AddSeparator(const ACaption: string = '');
    procedure AddEmptyLine;

    procedure AddSchemaTable(ATable: TDBTable; AWriter: TBaseRDBMSWriter);
    procedure AddCmdParameters(APars: TParameters);

    property LogFile: string read FLogFile write SetLogFile;
    property Control: TMemo read FControl write FControl;
  end;

var
  Log: TLog;

implementation

uses SysUtils, UnProcAll, DateUtils, Variants, StrUtils, Forms, TypInfo;

{ TLog }

procedure TLog.Add(const AKind: TLogKind; const AMessage: string);
var
  s: string;
begin
  if AKind = lkInfo then
    s := AMessage
  else if AKind = lkError then
    s := '[Error] ' + AMessage
  else if AKind = lkSuccess then
    s := '[Success] ' + AMessage;
  Writeln(FTextFile, s);
  if Assigned(FControl) then begin
    FControl.Lines.Add(s);
    Application.ProcessMessages;
  end;
end;

procedure TLog.Add(const AErrorKind: TLogErrorKind; const ACode: Integer; const AMessage: string);
begin
  Add(lkError, Format('Kind = %s, Code = %d, Text = %s', [GetEnumName(TypeInfo(TLogErrorKind), Ord(AErrorKind)), ACode, AMessage]));
end;

procedure TLog.AddCmdParameters(APars: TParameters);
var
  i: Integer;
begin
  try
    AddSeparator('Command parameters');
    for i := 0 to APars.Count - 1 do
      if VarIsNull(APars[i].Value) then
        Add(lkInfo, Format('Name = %s, Type = %s, Value = NULL', [APars[i].Name, DataTypeToString(APars[i].DataType)]))
      else
        Add(lkInfo, Format('Name = %s, Type = %s, Value = %s', [APars[i].Name, DataTypeToString(APars[i].DataType), VarToStr(APars[i].Value)]));
    AddSeparator;
  except
    on e: Exception do
      Add(lkError, 'AddCmdParameters is broken. => ' + e.Message);
  end;
end;

procedure TLog.AddEmptyLine;
begin
  Add(lkInfo, '');
end;

procedure TLog.AddSeparator(const ACaption: string = '');
const
  len = 80;
var
  s, s1: string;
  i: Integer;
begin
  s := StringOfChar('=', len);

  if ACaption <> '' then begin
    s1 := ' ' + Trim(ACaption) + ' ';
    i := (len div 2) - (Length(s1) div 2);
    if i > 1 then begin
      insert(s1, s, i);
      SetLength(s, len);
    end
    else
      s := Format('== %s ==', [s1]);
  end;

  Add(lkInfo, s);
end;

procedure TLog.AddSchemaTable(ATable: TDBTable; AWriter: TBaseRDBMSWriter);
var
  i: Integer;
begin
  try
    AddSeparator(Format('Schema of table %s', [ATable.Name]));
    for i := 0 to ATable.Count - 1 do begin
      Add(lkInfo, Format('field %s, FF: %s, %s: %s, Units %d, Dec %d, %s',
        [ATable[i].Name, ATable[i].TypeName, AWriter.TypeName, AWriter.GetDataTypeName(ATable[i]),
        ATable[i].Units, ATable[i].Decimals,
          IfThen(ATable[i].HasDefault, 'default: ' + ATable[i].Default_, 'no default')
          ]));
    end;
    AddEmptyLine;
    Add(lkInfo, 'Create statement: ');
    AWriter.LogSQL_TableCreate(ATable);
    Add(lkInfo, 'Insert statement: ');
    AWriter.LogSQL_RecordInsert(ATable);
    AddSeparator;
  except
    on e: Exception do
      Add(lkError, 'AddSchemaTable is broken. => ' + e.Message);
  end;
end;

procedure TLog.Clear;
begin
  ReWrite(FTextFile); {Create a new file }
  if Assigned(FControl) then
    FControl.Lines.Clear;
end;

constructor TLog.Create;
begin
  FControl := nil;

  FLogFile := GetSpecialPathLocation + 'ff-rdbms-' + FormatDateTime('yyyy-mm-dd-hh-MM-ss', Now) + '.log';
  AssignFile(FTextFile, FLogFile); {Assigns the Filename}
  ReWrite(FTextFile);
end;

destructor TLog.Destroy;
begin
  Closefile(FTextFile); {Closes file F}
  inherited;
end;

procedure TLog.SetLogFile(AValue: string);
begin
  if FLogFile <> AValue then begin
    CloseFile(FTextFile);
    FLogFile := AValue;
    AssignFile(FTextFile, FLogFile); {Assigns the Filename}
  end;
end;

end.

