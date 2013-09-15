unit UnBaseRDBMSWriter;

interface

uses UnSourceDatabase, ADODB, DB, ffdb, ffllbase, Classes, UnProcAll;

const
  { ConnectionString flags }
  CS_Host = 1; { ConnectionString. Uses a host }
  CS_Port = 2; { ConnectionString. Uses a port }
  CS_DB = 4; { ConnectionString. Uses a database }
  CS_UserName = 8; { ConnectionString. Uses a username }
  CS_Password = 16; { ConnectionString. Uses a password }
  CS_Trusted = 32; { ConnectionString. Uses a trusted connection }

type
  TBaseRDBMSWriter = class;
  TWriterStatistic = class;

  TCS_Rec = record
    Host: string;
    Port: string;
    DB: string;
    UserName: string;
    Password: string;
    Trusted: Boolean;
  end;

  TBaseRDBMSWriter = class
  private
    FADOConnection: TADOConnection;
    FADOCommandInsert: TADOCommand;
    FADOCommandCreate: TADOCommand;
    FADOQuery: TADOQuery;
    FStatistic: TWriterStatistic;
    FCanceled: Boolean;

    FUseNonUnicodeTypes: Boolean;
    FUseIdentity: Boolean;

    FProgressMainEvent: TProgressEvent;
    FProgressDetailEvent: TProgressEvent;

    procedure DoProgressMain(const AIndex: Integer);
    procedure DoProgressDetail(const AIndex: Integer);

    function GetIsConnected: Boolean;
  protected
    FUseLowerCase: Boolean;
    FReservedPrimaryKeys: TStringList;
    { used for connect with SQL Server via ADO }
    property Conn: TADOConnection read FADOConnection;
    { used for insert of data }
    property CmdInsert: TADOCommand read FADOCommandInsert;
    { used for drop and create table&indexes }
    property CmdCreate: TADOCommand read FADOCommandCreate;
    property CmdQuery: TADOQuery read FADOQuery;

    function DoCmdCreate(const ASQLString: string): Boolean;
    function DoCmdInsert(const ARowIndex: Integer): Boolean;
    function DoCmdTransferData(APar: TParameter; AQField: TField; AField: TDBField; const ARowIndex: Integer): Boolean;
    function GetCorrectStream(ADataSet: TffTable; AField: TField; AFieldType: TffFieldType; AStream: TMemoryStream): Boolean;
    procedure GetReservedPrimaryKeys; virtual;

    procedure SaveStreamToParameter(APar: TParameter; AFieldType: TffFieldType; AStream: TMemoryStream); virtual;

    class function GetTypeName: string; virtual; abstract; { short name }
    class function GetTypeFullName: string; virtual; abstract; { full name }
    class function GetCS_Flags: Integer; virtual; abstract; // is used to convert a ConnectionStringRecord to a ConnectionString
    function GetCanUseNonUnicodeTypes: Boolean; virtual;
    function GetCanUseIdentity: Boolean; virtual;
    { to get sql strings }
    function GetSQL_Select(ADBTable: TDBTable): string; virtual;
    function GetSQL_TableCreate(ADBTable: TDBTable): string; virtual;
    function GetSQL_TableDeleteIfExists(ADBTable: TDBTable): string; virtual; abstract;
    procedure GetSQL_TableAddPrimaryKey(ADBTable: TDBTable; var ASQL_SetNotNull, ASQL_AddPrimaryKey: string); virtual;
    function GetSQL_Insert(ADBTable: TDBTable): string; virtual;
    function GetSQL_IndexDeleteIfExists(ADBIndex: TDBIndex): string; virtual; abstract;
    function GetSQL_IndexCreate(ADBIndex: TDBIndex): string; virtual;
    function GetScript_EndOfBatch: string; virtual;
    function GeneratePrimaryKeyName(const ATemplateName: string): string; virtual;
    { to enable\disable some controls, such as to edit an identity }
    procedure BeginUpdate(ADBTable: TDBTable); virtual;
    procedure EndUpdate(ADBTable: TDBTable); virtual;
    function PrimaryKeyIsValid(ADBTable: TDBTable): Boolean; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property TypeName: string read GetTypeName;
    property TypeFullName: string read GetTypeFullName;
    property CS_Flags: Integer read GetCS_Flags;

    { to work with types }
    function GetDataTypeName(ADBField: TDBField): string; virtual; abstract;
    { to log some useful information }
    procedure LogSQL_TableCreate(ATable: TDBTable); virtual;
    procedure LogSQL_RecordInsert(ATable: TDBTable); virtual;

    function GetConnectionString(const ACS_Rec: TCS_Rec): string; virtual;
    function Connect(const AConnectionString: string): Boolean; overload;
    function Connect(const AConnectionStringRec: TCS_Rec): Boolean; overload;
    procedure Disconnect;

    { step #1 Creating schema of tables }
    procedure DoCreateSchemaOfTables(ADB: TSourceDatabase);
    { step #2 Transferring a set of data }
    procedure DoTransferData(ADB: TSourceDatabase; AGetData: TGetSourceTable; const ARowLimit: Integer = 0);
    { step #3 Creating primary keys }
    procedure DoCreatePrimaryKeys(ADB: TSourceDatabase);
    { step #4 Creating indexes of tables }
    procedure DoCreateIndexesOfTables(ADB: TSourceDatabase);
    { to cancel the process }
    procedure Cancel;

    { making scripts }
    procedure SaveScriptSchemaOfTables(ADatabase: TSourceDatabase); virtual;
    procedure SaveScriptIndexesOfTables(ADatabase: TSourceDatabase); virtual;

    property Statistic: TWriterStatistic read FStatistic;
    property ProgressMainEvent: TProgressEvent read FProgressMainEvent write FProgressMainEvent;
    property ProgressDetailEvent: TProgressEvent read FProgressDetailEvent write FProgressDetailEvent;
    property IsConnected: Boolean read GetIsConnected;
    { cancelled by user }
    property Canceled: Boolean read FCanceled;
    { Use Non-Unicode Types}
    property CanUseNonUnicodeTypes: Boolean read GetCanUseNonUnicodeTypes;
    property UseNonUnicodeTypes: Boolean read FUseNonUnicodeTypes write FUseNonUnicodeTypes;
    { to convert all identifiers (table names, column names, index names, etc.) to lower case }
    property UseLowerCase: Boolean read FUseLowerCase write FUseLowerCase;
    { sequence }
    property CanUseIdentity: Boolean read GetCanUseIdentity;
    property UseIdentity: Boolean read FUseIdentity write FUseIdentity;
  end;

  TWriterStatistic = class
  private
    FTables: Integer;
    FFields: Integer;
    FRows: Integer;
    FSourceRows: Integer;
    FIndexes: Integer;
    FTimeStart: Cardinal;
    FErrors: Integer;
  public
    constructor Create;
    procedure Clear;
    procedure DoLog(var AResultNames: string; var AResultValues: string);

    property Tables: Integer read FTables;
    property Fields: Integer read FFields;
    property Rows: Integer read FRows;
    property SourceRows: Integer read FSourceRows;
    property Indexes: Integer read FIndexes;
    property TimeStart: cardinal read FTimeStart;
    property Errors: Integer read FErrors;
  end;

implementation

uses SysUtils, UnLog, Variants, ExtCtrls, Graphics, Windows, Math, StrUtils;

{ TBaseRDBMSWriter }

function TBaseRDBMSWriter.DoCmdTransferData(APar: TParameter;
  AQField: TField; AField: TDBField; const ARowIndex: Integer): Boolean;
var
  sm: TMemoryStream;

  function IsDateTime(AField: TField): Boolean;
  var
    buffer: array[0..dsMaxStringSize] of Byte;
    data: TDateTimeRec;
    timeStamp: TTimeStamp;
  begin
    Result := False;
    if AField.DataSet.GetFieldData(AField, @buffer) then begin
      data := TDateTimeRec(Pointer(@buffer)^);

      if AField.DataType = ftDate then begin
        TimeStamp.Time := 0;
        TimeStamp.Date := Data.Date;
      end
      else
        try
          timeStamp := MSecsToTimeStamp(Data.DateTime);
        except
          timeStamp.Time := 0;
          timeStamp.Date := 0;
        end;

      Result := timeStamp.Date > 0;
    end;
  end;
begin
  Result := False;
  try
    if not AQField.IsBlob then begin
      if not (AQField.DataType in [ftDate, ftDateTime]) then
        APar.Value := AQField.Value
      else
        if IsDateTime(AQField) then
          APar.Value := AQField.AsString
        else
          APar.Value := null;
      Result := True;
    end
    else begin
      sm := TMemoryStream.Create;
      try
        Result := GetCorrectStream(TffTable(AQField.DataSet), AQField, AField.Type_, sm);
        if Result then
          if sm.Size <> 0 then
            SaveStreamToParameter(APar, AField.Type_, sm)
          else
            APar.Value := null;
      finally
        FreeAndNil(sm);
      end;
    end;
  except
    on e: Exception do
      Log.Add(lkError, e.Message);
  end;

  if not Result then
    try
      Log.Add(lkError, Format('Transfer of data has been failed => [Table = %s, Field = %s, RowIndex = %d]',
        [AField.Table.Name, AField.Name, ARowIndex + 1]));
      Log.Add(lkError, Format('[%s] FF type = %s, %s type = %s, Par.Type = %s, FFQuery.Type = %s, IsBlob = %s',
        [AField.Name, AField.TypeName, GetTypeName, GetDataTypeName(AField),
        DataTypeToString(APar.DataType), DataTypeToString(AQField.DataType), BoolToStr(AQField.IsBlob, True)]));
      if not AQField.IsBlob then
        Log.Add(lkError, Format('[%s] Value = %s', [AField.Name, VarToStrDef(AQField.AsString, 'Can not get value')]));
    except
      on e: Exception do
        Log.Add(lkError, e.Message);
    end;
end;

function TBaseRDBMSWriter.GetCorrectStream(ADataSet: TffTable; AField: TField; AFieldType: TffFieldType; AStream: TMemoryStream): Boolean;
const
  JPEGHeader: array[0..10] of Char = (Chr($FF), Chr($D8), Chr($FF), Chr($E0),
    Chr($0), Chr($10), 'J', 'F', 'I', 'F', Chr(0));
  BMPHeader: array[0..1] of char = ('B', 'M');
var
  ByteArrayBuffer: Pointer;
  HeaderBuffer: array[0..10] of Char;
  stream: TMemoryStream;

  { attempt to load Icon }
  function IconLoadFromStream(AImage: TImage; AStream: TMemoryStream): Boolean;
  begin
    Result := True;
    
    AStream.Position := 8;
    try
      AImage.Picture.Icon.LoadFromStream(AStream);
    except
      on EInvalidGraphic do begin
        AStream.Position := 0;
        try
          AImage.Picture.Icon.LoadFromStream(AStream);
        except
          on EInvalidGraphic do
          else begin
            Log.Add(ekFFConvertBlob, 2, '');
            Result := False;
          end;
        end;
      end
      else begin
        Log.Add(ekFFConvertBlob, 3, '');
        Result := False;
      end;
    end;
  end;

  { attempt to load Metafile }
  function  MetafileLoadFromStream(AStream: TMemoryStream): Boolean;
  var
    image: TImage;
  begin
    Result := True;

    image := TImage.Create(nil);
    AStream.Position := 8;
    try
      image.Picture.Metafile.LoadFromStream(AStream);
    except
      on EInvalidGraphic do begin
        AStream.Position := 0;
        try
          image.Picture.Metafile.LoadFromStream(AStream);
        except
          on EInvalidGraphic do begin {icon?}
            { it's difficult to check for the icon type. we just
             attempt to load and let the TImage component find out. }
            Result := IconLoadFromStream(image, AStream);
          end
          else begin
            Log.Add(ekFFConvertBlob, 4, '');
            Result := False;
          end;
        end;
      end
      else begin
        Log.Add(ekFFConvertBlob, 5, '');
        Result := False;
      end;
    end;
    FreeAndNil(image);
  end;
begin
  Result := True;

  stream := nil;
  case AFieldType of
    fftBLOBGraphic:
      begin
        try
          stream := TMemoryStream(ADataSet.CreateBlobStream(AField, bmRead));

          { data in stream? }
          if stream.Read(HeaderBuffer, 11) = 11 then
            if CompareMem(@jpegHeader, @HeaderBuffer, 11) then { jpg? }
              stream.Position := 0
            else if CompareMem(@BMPHeader, @HeaderBuffer, 2) or CompareMem(@BMPHeader, @HeaderBuffer[8], 2) then {bmp?}
              if CompareMem(@BMPHeader, @HeaderBuffer, 2) then
                stream.Position := 0
              else
                stream.Position := 8
            else begin {metafile?}
              { it's difficult to check for the metafile type. we just
                attempt to load and let the TImage component find out.  }
              Result := MetafileLoadFromStream(stream);
            end;
        except
          on e: Exception do begin
            Log.Add(ekFFConvertBlob, 0, 'Exception: ' + e.Message + ' decoding graphic field: ' + AField.FieldName);
            Result := False;
          end;
        end;
      end;
    fftByteArray:
      begin
        GetMem(ByteArrayBuffer, AField.DataSize);
        try
          if AField.GetData(ByteArrayBuffer) then begin
            stream := TMemoryStream.Create;
            stream.Write(ByteArrayBuffer, AField.DataSize);
            stream.Position := 0;
          end;
        except
          on e: Exception do begin
            Log.Add(ekFFConvertBlob, 1, 'Exception: ' + e.Message + ' when receiving byte array field: ' + AField.FieldName);
            Result := False;
          end;
        end;
        FreeMem(ByteArrayBuffer);
      end;
    fftBLOB,
      fftBLOBOLEObj,
      fftBLOBDBSOLEObj,
      fftBLOBTypedBin,
      fftBLOBFile,
      fftBLOBMemo,
      fftBLOBFmtMemo:
      begin
        try
          stream := TMemoryStream(ADataSet.CreateBlobStream(AField, bmRead));
        except
          on e: Exception do begin
            Log.Add(ekFFConvertBlob, 1, 'Exception: ' + e.Message + ' when receiving blob field: ' + AField.FieldName);
            Result := False;
          end;
        end;
      end;
  end;

  if Assigned(stream) then begin
    if stream.Size - stream.Position > 0 then begin
      AStream.CopyFrom(stream, stream.Size - stream.Position);
      AStream.Position := 0;
    end;
    FreeAndNil(stream);
  end;
end;

function TBaseRDBMSWriter.Connect(const AConnectionStringRec: TCS_Rec): Boolean;
begin
  Result := Connect(GetConnectionString(AConnectionStringRec));
end;

function TBaseRDBMSWriter.Connect(const AConnectionString: string): Boolean;
begin
  Conn.Close;
  Conn.ConnectionString := AConnectionString;
  try { will try to connect to ms sql server }
    Conn.Open;
    GetReservedPrimaryKeys;

    Result := stOpen in Conn.State;
  except
    Result := False;
  end;
end;

constructor TBaseRDBMSWriter.Create;
begin
  FCanceled := False;

  FADOConnection := TADOConnection.Create(nil);
  FADOConnection.LoginPrompt := False;
  FADOCommandInsert := TADOCommand.Create(nil);
  FADOCommandInsert.Connection := FADOConnection;
  FADOCommandInsert.ParamCheck := True;
  FADOCommandInsert.ExecuteOptions := [eoExecuteNoRecords];
  FADOCommandCreate := TADOCommand.Create(nil);
  FADOCommandCreate.Connection := FADOConnection;
  FADOCommandCreate.ParamCheck := False;
  FADOCommandCreate.ExecuteOptions := [eoExecuteNoRecords];
  FADOQuery := TADOQuery.Create(nil);
  FADOQuery.Connection := FADOConnection;

  FReservedPrimaryKeys := TStringList.Create;
  FReservedPrimaryKeys.Sorted := True;

  FStatistic := TWriterStatistic.Create;
  FProgressMainEvent := nil;
  FProgressDetailEvent := nil;
  FUseNonUnicodeTypes := False;
  FUseLowerCase := False;
  FUseIdentity := True;
end;

destructor TBaseRDBMSWriter.Destroy;
begin
  FreeAndNil(FStatistic);
  
  FreeAndNil(FReservedPrimaryKeys);

  FreeAndNil(FADOQuery);
  FreeAndNil(FADOCommandCreate);
  FreeAndNil(FADOCommandInsert);
  FreeAndNil(FADOConnection);
  inherited;
end;

procedure TBaseRDBMSWriter.Disconnect;
begin
  Conn.Close;
end;

function TBaseRDBMSWriter.DoCmdCreate(const ASQLString: string): Boolean;
begin
  Result := False;
  try
    CmdCreate.CommandText := ASQLString;
    CmdCreate.Execute;
    Result := True;
  except
    on e: Exception do begin
      Log.Add(ekSQLCreateFailed, 0, ASQLString);
      Log.Add(lkError, e.Message);
    end;
  end;
end;

function TBaseRDBMSWriter.DoCmdInsert(const ARowIndex: Integer): Boolean;
begin
  Result := False;
  try
    CmdInsert.Execute;
    Result := True;
  except
    on e: Exception do begin
      Log.Add(ekSQLInsertFailed, 0, e.Message);
      Log.Add(lkError, Format('Incorrect row => [RowIndex = %d]', [ARowIndex + 1]));
      Log.AddCmdParameters(CmdInsert.Parameters);
    end;
  end;
end;

procedure TBaseRDBMSWriter.DoCreateSchemaOfTables(ADB: TSourceDatabase);
var
  i: Integer;
  t: cardinal;
begin
  Log.Add(lkInfo, Format('Create %s tables', [TypeFullName]));

  t := GetTickCount;
  DoProgressMain(0);
  for i := 0 to ADB.Count - 1 do begin
    if Canceled then
      Break;

    Log.AddSchemaTable(ADB[i], Self);
    Log.Add(lkInfo, 'Transferring schema for table ' + ADB[i].Name);
    if not DoCmdCreate(GetSQL_TableDeleteIfExists(ADB[i])) then begin
      inc(Statistic.FErrors);
      Log.Add(lkError, 'Cannot drop existing table');
      Continue;
    end;
    if not DoCmdCreate(GetSQL_TableCreate(ADB[i])) then begin
      inc(Statistic.FErrors);
      Log.Add(lkError, 'Cannot create table');
      Continue;
    end;
    inc(Statistic.FTables);
    inc(Statistic.FFields, ADB[i].Count);
    Log.Add(lkSuccess, 'Done');

    if GetTickCount - t > 1000 then begin
      DoProgressMain(((i + 1) * 100) div ADB.Count);
      t := GetTickCount;
    end;
  end;
  if not Canceled then
    Log.Add(lkInfo, Format('Tables have been created (%d error(s))', [Statistic.errors]));
end;

procedure TBaseRDBMSWriter.DoProgressDetail(const AIndex: Integer);
begin
  if Assigned(FProgressDetailEvent) then
    FProgressDetailEvent(AIndex);
end;

procedure TBaseRDBMSWriter.DoProgressMain(const AIndex: Integer);
begin
  if Assigned(FProgressMainEvent) then
    FProgressMainEvent(AIndex);
end;

procedure TBaseRDBMSWriter.DoTransferData(ADB: TSourceDatabase; AGetData: TGetSourceTable; const ARowLimit: Integer = 0);
const
  tran_count = 1000;
var
  i, j, full_count, k, er: Integer;
  q: TffTable;
  t, t_loop: Cardinal;

  function FFTypeToDelphiType(AFFType: TffFieldType): TDataType;
  begin
    case AFFType of
      fftBoolean: Result := ftBoolean;
      fftByte, fftInt8, fftInt16: Result := ftSmallint;
      fftWord16, fftInt32, fftAutoInc: Result := ftInteger;
      fftWord32, fftComp: Result := ftLargeint;
      fftSingle, fftDouble, fftExtended: Result := ftFloat;
      fftBLOBMemo, fftBLOBFmtMemo: Result := ftMemo;
      fftBLOB, fftBLOBOLEObj, fftBLOBGraphic,
        fftBLOBDBSOLEObj, fftBLOBTypedBin,
        fftBLOBFile, fftByteArray: Result := ftBlob;
      fftChar, fftWideChar, fftCurrency,
        fftStDate, fftStTime, fftDateTime,
        fftShortString, fftShortAnsiStr,
        fftNullString, fftNullAnsiStr,
        fftWideString: Result := ftString;
    else
      Result := ftUnknown;
    end;
  end;
begin
  Log.AddSeparator('Converting data');
  DoProgressMain(0);
  for i := 0 to ADB.Count - 1 do begin
    if Canceled then
      Break;
    // ---
    Log.Add(lkInfo, 'Transferring data for table ' + ADB[i].Name);
    // --
    q := AGetData(ADB[i]);
    if not Assigned(q) then begin
      Log.Add(lkError, 'Can not open FF dataset such as: ' + GetSQL_Select(ADB[i]));
      inc(Statistic.FErrors);
      Continue;
    end;
    Log.Add(lkInfo, 'FF dataset has been opened');
    // --
    full_count := q.RecordCount;
    if ARowLimit <> 0 then begin
      Log.Add(lkInfo, Format('Count of Rows = %d (Max = %d)', [ARowLimit, full_count]));
      full_count := Min(full_count, ARowLimit);
    end
    else
      Log.Add(lkInfo, Format('Count of Rows = %d', [full_count]));
    inc(Statistic.FSourceRows, full_count);
    // --
    try
      CmdInsert.CommandText := GetSQL_Insert(ADB[i]);

      for j := 0 to ADB[i].Count - 1 do
        CmdInsert.Parameters[j].DataType := FFTypeToDelphiType(ADB[i][j].Type_);

      CmdInsert.Prepared := True;
    except
      on e: Exception do begin
        Log.Add(lkError, 'The dataset for an insertion of data is broken. ' + GetSQL_Insert(ADB[i]));
        Log.Add(lkError, e.Message);
        inc(Statistic.FErrors);
        Continue;
      end;
    end;

    BeginUpdate(ADB[i]);

    t := GetTickCount;
    t_loop := GetTickCount;
    k := 0;
    er := Statistic.errors;
    Conn.BeginTrans;
    while not q.Eof and ((ARowLimit <= 0) or (ARowLimit > 0) and (k < ARowLimit)) do begin
      if Canceled then
        Break;
      // ---
      for j := 0 to q.Fields.Count - 1 do
        if not DoCmdTransferData(CmdInsert.Parameters[j], q.Fields[j], ADB[i][j], k) then begin
          inc(Statistic.FErrors);
          Break;
        end;
      if er <> Statistic.errors then
        Break;
      // ----
      if DoCmdInsert(k) then
        inc(Statistic.FRows)
      else begin
        inc(Statistic.FErrors);
        Break;
      end;

      if (k <> 0) and ((k + 1) mod tran_count = 0) then begin
        try
          Conn.CommitTrans;
          Log.Add(lkInfo, Format('%d of %d row done', [k + 1, full_count]));
        except
          on e: Exception do begin
            Log.Add(lkError, Format('Commit transactions is failed. [RowIndex = %d, TransactionStep = %d]', [k + 1, tran_count]));
            Log.Add(lkError, e.Message);
            inc(Statistic.FErrors);
            Conn.RollbackTrans;
            Break;
          end;
        end;
        Conn.BeginTrans;
      end;

      if GetTickCount - t > 1000 then begin
        DoProgressDetail(((k + 1) * 100) div full_count);
        t := GetTickCount;
      end;
      q.Next;
      inc(k);
    end;

    if (er = Statistic.errors) and not Canceled then begin
      try
        Conn.CommitTrans;
        Log.Add(lkInfo, Format('%d of %d row done', [k, full_count]));
        t_loop := (GetTickCount - t_loop) div 1000;
        if t_loop <> 0 then
          Log.Add(lkInfo, Format('Elapsed time: %s. %d rows per second.',
            [FormatDateTime('hh:mm:ss', EncodeTime((t_loop mod 86400) div 3600, (t_loop mod 3600) div 60, t_loop mod 60, 0)),
            Trunc(full_count / t_loop)]));
      except
        on e: Exception do begin
          Log.Add(lkError, Format('%d of %d row failed', [k, full_count]));
          Log.Add(lkError, e.Message);
          inc(Statistic.FErrors);
          Conn.RollbackTrans;
          Break;
        end;
      end;
    end
    else begin
      Conn.RollbackTrans;
      if Canceled then
        Log.Add(lkInfo, 'Cancelled by user');
    end;
    EndUpdate(ADB[i]);
    q.Close;
    // ---
    DoProgressMain(((i + 1) * 100) div ADB.Count);
  end;
end;

procedure TBaseRDBMSWriter.DoCreatePrimaryKeys(ADB: TSourceDatabase);
var
  i: Integer;
  s_pk, s_notnull: string;
begin
  Log.AddSeparator(Format('Create %s primary keys', [TypeFullName]));

  for i := 0 to ADB.Count - 1 do begin
    if Canceled then
      Break;

    if PrimaryKeyIsValid(ADB[i]) then begin
      Log.Add(lkInfo, 'Creating primary key for table ' + ADB[i].Name);

      GetSQL_TableAddPrimaryKey(ADB[i], s_notnull, s_pk);
      if not (((s_notnull = '') or DoCmdCreate(s_notnull)) and DoCmdCreate(s_pk)) then begin
        Log.Add(lkError, 'Cannot create primary key: ' + s_notnull + #13#10 + s_pk);
        inc(Statistic.FErrors);
        Continue;
      end;
    end;
  end;

  if not Canceled then
    Log.Add(lkInfo, Format('Primary keys have been created (%d error(s))', [statistic.errors]));
  Log.AddSeparator;
end;

procedure TBaseRDBMSWriter.DoCreateIndexesOfTables(ADB: TSourceDatabase);
var
  i, j: Integer;
begin
  Log.AddSeparator(Format('Create %s indexes', [TypeFullName]));

  for i := 0 to ADB.Count - 1 do begin
    if Canceled then
      Break;

    Log.Add(lkInfo, 'Try to create indexes for table ' + ADB[i].Name);
    for j := 0 to ADB[i].Indexes.Count - 1 do begin
      if Canceled then
        Break;

      if not DoCmdCreate(GetSQL_IndexDeleteIfExists(ADB[i].Indexes[j])) then begin
        Log.Add(lkError, 'Cannot drop existing index');
        inc(Statistic.FErrors);
        Continue;
      end;
      if not DoCmdCreate(GetSQL_IndexCreate(ADB[i].Indexes[j])) then begin
        Log.Add(lkError, 'Cannot create index: ' + GetSQL_IndexCreate(ADB[i].Indexes[j]));
        inc(Statistic.FErrors);
        Continue;
      end;
      inc(Statistic.FIndexes);
    end;
    Log.Add(lkSuccess, 'Done');
  end;

  if not Canceled then
    Log.Add(lkInfo, Format('Indexes have been created (%d error(s))', [statistic.errors]));
end;

function TBaseRDBMSWriter.GetIsConnected: Boolean;
begin
  Result := stOpen in FADOConnection.State;
end;

function TBaseRDBMSWriter.GetSQL_Select(ADBTable: TDBTable): string;
var
  i: Integer;
begin
  Result := '';
  if ADBTable.Count = 0 then
    Exit;

  if ADBTable.Count > 0 then
    Result := '"' + ADBTable[0].Name + '"';
  for i := 1 to ADBTable.Count - 1 do
    Result := Result + ',"' + ADBTable[i].Name + '"';
  Result := Format('select %s from "%s";', [Result, ADBTable.Name]);
end;

function TBaseRDBMSWriter.GetSQL_TableCreate(ADBTable: TDBTable): string;
var
  i: Integer;

  function GetSQL_FieldDescription(ADBField: TDBField): string;
  begin
    if ADBField.HasDefault then
      Result := Format('"%s" %s default ''%s''', [ADBField.Name, GetDataTypeName(ADBField), ADBField.Default_])
    else
      Result := Format('"%s" %s', [ADBField.Name, GetDataTypeName(ADBField)])
  end;
begin
  if ADBTable.Count <> 0 then begin
    Result := Format('create table "%s" (', [ADBTable.Name]) + #13#10;
    for i := 0 to ADBTable.Count - 1 do
      if i <> ADBTable.Count - 1 then
        Result := Result + '   ' + GetSQL_FieldDescription(ADBTable[i]) + ', ' + #13#10
      else
        Result := Result + '   ' + GetSQL_FieldDescription(ADBTable[i]) + ');';
  end
  else Result := '';
end;

function TBaseRDBMSWriter.GetSQL_Insert(ADBTable: TDBTable): string;
var
  i: Integer;
  s1, s2: string;
begin
  Result := '';
  if ADBTable.Count = 0 then
    Exit;

  s1 := '';
  s2 := '';

  if ADBTable.Count > 0 then begin
    s1 := '"' + ADBTable[0].Name + '"';
    s2 := ':"' + ADBTable[0].Name + '"'
  end;
  for i := 1 to ADBTable.Count - 1 do begin
    s1 := s1 + ', "' + ADBTable[i].Name + '"';
    s2 := s2 + ', :"' + ADBTable[i].Name + '"'
  end;
  Result := Format('insert into "%s" (%s) values (%s);', [ADBTable.Name, s1, s2]);
end;

procedure TBaseRDBMSWriter.LogSQL_RecordInsert(ATable: TDBTable);
begin
  Log.Add(lkInfo, GetSQL_Insert(ATable));
end;

procedure TBaseRDBMSWriter.LogSQL_TableCreate(ATable: TDBTable);
var
  s_pk, s_notnull: string;
begin
  Log.Add(lkInfo, GetSQL_TableCreate(ATable));
  GetSQL_TableAddPrimaryKey(ATable, s_notnull, s_pk);
  if s_notnull <> '' then
    Log.Add(lkInfo, s_notnull);
  Log.Add(lkInfo, s_pk);
end;

function TBaseRDBMSWriter.GetSQL_IndexCreate(ADBIndex: TDBIndex): string;
var
  i: Integer;
begin
  Result := '';
  if ADBIndex.Count = 0 then
    Exit;

  if ADBIndex.Count > 0 then
    Result := '"' + ADBIndex[0].Name + '"';
  for i := 1 to ADBIndex.Count - 1 do
    Result := Result + ', "' + ADBIndex[i].Name + '"';

  if ADBIndex.Unique then
    Result := Format('create unique index "%s" on "%s" (%s);', [ADBIndex.Name, ADBIndex.Indexes.Table.Name, Result])
  else
    Result := Format('create index "%s" on "%s" (%s);', [ADBIndex.Name, ADBIndex.Indexes.Table.Name, Result]);
end;

procedure TBaseRDBMSWriter.BeginUpdate(ADBTable: TDBTable);
begin

end;

procedure TBaseRDBMSWriter.EndUpdate(ADBTable: TDBTable);
begin

end;

procedure TBaseRDBMSWriter.SaveStreamToParameter(APar: TParameter; AFieldType: TffFieldType; AStream: TMemoryStream);
begin
  if AFieldType in [fftBLOBMemo, fftBLOBFmtMemo] then
    APar.LoadFromStream(AStream, ftMemo)
  else
    APar.LoadFromStream(AStream, ftBlob);
end;

function TBaseRDBMSWriter.GetConnectionString(const ACS_Rec: TCS_Rec): string;
begin
  Result := '';
end;

procedure TBaseRDBMSWriter.SaveScriptIndexesOfTables(ADatabase: TSourceDatabase);
var
  s: string;
  f: Textfile;
  i, j: Integer;
begin
  s := StringReplace(Log.LogFile, '.log', '', [rfIgnoreCase]) + '-indexes.sql';
  try
    AssignFile(f, s); {Assigns the Filename}
    ReWrite(f);
    // ---
    Writeln(f, '-- ' + ClassName);
    Writeln(f, '-- Begin');
    Writeln(f);
    for i := 0 to ADatabase.Count - 1 do
      if ADatabase[i].Indexes.Count <> 0 then begin
        Writeln(f, Format('--Table "%s"', [ADatabase[i].Name]));
        for j := 0 to ADatabase[i].Indexes.Count - 1 do begin
          Writeln(f, GetSQL_IndexDeleteIfExists(ADatabase[i].Indexes[j]));
          Writeln(f, GetSQL_IndexCreate(ADatabase[i].Indexes[j]));
          Writeln(f);
        end;
      end;
    Writeln(f, '-- End');
    // ---
    Closefile(f);
  except
    Log.Add(lkError, 'Can not save the SQL for index creation in a SQL script.');
  end;
end;

procedure TBaseRDBMSWriter.SaveScriptSchemaOfTables(ADatabase: TSourceDatabase);
var
  s, s_pk, s_notnull: string;
  f: Textfile;
  i: Integer;
begin
  s := StringReplace(Log.LogFile, '.log', '', [rfIgnoreCase]) + '-tables.sql';
  try
    AssignFile(f, s); {Assigns the Filename}
    ReWrite(f);
    // ---
    Writeln(f, '-- ' + ClassName);
    Writeln(f, '-- Begin');
    Writeln(f);
    for i := 0 to ADatabase.Count - 1 do begin
      Writeln(f, Format('--Table "%s"', [ADatabase[i].Name]));
      Writeln(f, GetSQL_TableDeleteIfExists(ADatabase[i]));

      Writeln(f);
      Writeln(f, GetSQL_TableCreate(ADatabase[i]));
      GetSQL_TableAddPrimaryKey(ADatabase[i], s_notnull, s_pk);
      if s_notnull <> '' then begin
        Writeln(f, s_notnull);
        Writeln(f, GetScript_EndOfBatch);
      end;
      Writeln(f, s_pk);
      Writeln(f);
    end;
    Writeln(f, '-- End');
    // ---
    Closefile(f);
  except
    Log.Add(lkError, 'Can not save the SQL for table creation in a SQL script.');
  end;
end;

procedure TBaseRDBMSWriter.Cancel;
begin
  FCanceled := True;
end;

function TBaseRDBMSWriter.GetCanUseNonUnicodeTypes: Boolean;
begin
  Result := False;
end;

procedure TBaseRDBMSWriter.GetSQL_TableAddPrimaryKey(ADBTable: TDBTable; var ASQL_SetNotNull, ASQL_AddPrimaryKey: string);
var
  i: Integer;
begin
  ASQL_AddPrimaryKey := '';
  ASQL_SetNotNull := '';
  if ADBTable.PrimaryKeyCount > 0 then begin
    for i := 0 to ADBTable.PrimaryKeyCount - 1 do
      if i <> ADBTable.PrimaryKeyCount - 1 then
        ASQL_AddPrimaryKey := ASQL_AddPrimaryKey + Format('"%s"', [ADBTable.PrimaryKey[i].Name]) + ', '
      else
        ASQL_AddPrimaryKey := ASQL_AddPrimaryKey + Format('"%s"', [ADBTable.PrimaryKey[i].Name]);
    ASQL_AddPrimaryKey := Format('alter table "%0:s" add constraint "%2:s" primary key (%1:s);',
      [ADBTable.Name, ASQL_AddPrimaryKey, GeneratePrimaryKeyName(ADBTable.Name + '_pkey')]);
  end;
end;

function TBaseRDBMSWriter.PrimaryKeyIsValid(ADBTable: TDBTable): Boolean;
var
  k, i: Integer;

  function GetSQL_CheckNulls(ADBTable: TDBTable): string;
  var
    i: Integer;
  begin
    Result := 'select count(*) ';
    for i:=0 to ADBTable.PrimaryKeyCount-1 do
      Result := Result + Format(', count("%s")', [ADBTable.PrimaryKey[i].Name]);
    Result := Result + Format(' from "%s"', [ADBTable.Name]);
  end;

  function GetSQL_CheckDistinct(ADBTable: TDBTable): string;
  var
    i: Integer;
  begin
    if ADBTable.PrimaryKeyCount > 0 then
      Result := Format('"%s"', [ADBTable.PrimaryKey[0].Name]);

    for i:=1 to ADBTable.PrimaryKeyCount-1 do
      Result := Result + Format(', "%s"', [ADBTable.PrimaryKey[i].Name]);

    Result := Format('select (select count(*) from "%0:s"), count(*) from (select distinct %1:s from "%0:s") as A', [ADBTable.Name, Result]);
  end;
begin
  Result := ADBTable.PrimaryKeyCount <> 0;
  if Result then
    try
      CmdQuery.Close;
      CmdQuery.SQL.Text := GetSQL_CheckNulls(ADBTable);
      CmdQuery.Open;

      k := CmdQuery.Fields[0].AsInteger;
      for i:=1 to CmdQuery.Fields.Count-1 do
        if k <> CmdQuery.Fields[i].AsInteger then begin
          Result := False;
          Exit;
        end;

      CmdQuery.Close;
      CmdQuery.SQL.Text := GetSQL_CheckDistinct(ADBTable);
      CmdQuery.Open;

      k := CmdQuery.Fields[0].AsInteger;
      for i:=1 to CmdQuery.Fields.Count-1 do
        if k <> CmdQuery.Fields[i].AsInteger then begin
          Result := False;
          Exit;
        end;

      CmdQuery.Close;
    except
      on e: Exception do begin
        Log.Add(lkError, e.Message);
        Result := False;
      end;
    end;
end;

function TBaseRDBMSWriter.GetScript_EndOfBatch: string;
begin
  Result := '';
end;

procedure TBaseRDBMSWriter.GetReservedPrimaryKeys;
const
  sql_string = 'select distinct(constraint_name) from information_schema.table_constraints ' +
               'where constraint_type = ''PRIMARY KEY''';
begin
  FReservedPrimaryKeys.Clear;
  try
    CmdQuery.Close;
    CmdQuery.SQL.Text := sql_string;
    CmdQuery.Open;

    while not CmdQuery.Eof do begin
      FReservedPrimaryKeys.Add(CmdQuery.Fields[0].AsString);
      CmdQuery.Next;
    end;
  except
  end;
end;

function TBaseRDBMSWriter.GeneratePrimaryKeyName(const ATemplateName: string): string;
var
  k: Integer;
  s: string;
begin
  k := 0;
  s := ATemplateName;
  while (FReservedPrimaryKeys.IndexOf(s) <> -1) do begin
    s := Format('%s%d', [ATemplateName, k]);
    inc(k);
  end;
  Result := s;
end;

function TBaseRDBMSWriter.GetCanUseIdentity: Boolean;
begin
  Result := False;
end;

{ TWriterStatistic }

constructor TWriterStatistic.Create;
begin
  Clear;
end;

procedure TWriterStatistic.Clear;
begin
  FTables := 0;
  FFields := 0;
  FRows := 0;
  FSourceRows := 0;
  FIndexes := 0;
  FTimeStart := GetTickCount;
  FErrors := 0;
end;

procedure TWriterStatistic.DoLog(var AResultNames: string; var AResultValues: string);
var
  t: Cardinal;
begin
  t := (GetTickCount - TimeStart) div 1000;
  Log.Add(lkInfo, 'Statistic: ');
  Log.Add(lkInfo, Format('%d tables ', [Tables]));
  Log.Add(lkInfo, Format('%d fields ', [Fields]));
  Log.Add(lkInfo, Format('%d rows ', [Rows]));
  Log.Add(lkInfo, Format('%d indexes ', [Indexes]));
  Log.Add(lkInfo, Format('%d errors ', [Errors]));
  Log.Add(lkInfo, Format('Data verified: %s', [IfThen(SourceRows = Rows, 'Yes', 'No')]));
  Log.Add(lkInfo, Format('Elapsed time: %s', [FormatDateTime('hh:mm:ss', EncodeTime((t mod 86400) div 3600, (t mod 3600) div 60, t mod 60, 0))]));
                                                         
  AResultNames :=
    'Tables: ' + #13#10 +
    'Fields: ' + #13#10 +
    'Rows: ' + #13#10 +
    'Indexes: ' + #13#10 +
    'Data verified: ' + #13#10 +
    'Elapsed time: ' + #13#10 +
    'Errors: ';
  AResultValues :=
    IntToStr(Tables) + #13#10 +
    IntToStr(Fields) + #13#10 +
    IntToStr(Rows) + #13#10 +
    IntToStr(Indexes) + #13#10 +
    IfThen(SourceRows = Rows, 'Yes', 'No') + #13#10 +
    FormatDateTime('hh:mm:ss', EncodeTime((t mod 86400) div 3600, (t mod 3600) div 60, t mod 60, 0)) + #13#10 +
    IntToStr(Errors);
end;

end.

