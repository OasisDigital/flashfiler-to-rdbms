unit UnSourceDBLoader;

interface

uses UnSourceDatabase, ffsqleng, ffsreng, UndmBaseConn, ffdb, Grids,
  UnBaseRDBMSWriter;

type
  TSourceDBLoader = class
  private
    FDB: TSourceDatabase;
    FSQLEngine: TffSqlEngine;
    FServerEngine: TffServerEngine;

    FLoaded: Boolean;
    FConn: TdmBaseConn;
  public
    constructor Create;
    destructor Destroy; override;

    property SourceDB: TSourceDatabase read FDB;

    function Load(ADirPath: string): Boolean;
    procedure Bind(AGrid: TStringGrid; AWriter: TBaseRDBMSWriter);

    function GetTable(ADBTable: TDBTable): TffTable;

    property IsLoaded: Boolean read FLoaded;
  end;

implementation

uses SySUtils, Classes, Forms, UnProcAll, UnLog, DB;

{ TSourceDBLoader }

procedure TSourceDBLoader.Bind(AGrid: TStringGrid; AWriter: TBaseRDBMSWriter);
var
  i, k, j: Integer;
begin
  // ---
  AGrid.ColCount := 4;
  AGrid.ColWidths[0] := AGrid.Width div 4 - 7;
  AGrid.ColWidths[1] := AGrid.Width div 4 - 7;
  AGrid.ColWidths[2] := AGrid.Width div 4 - 7;
  AGrid.ColWidths[3] := AGrid.Width div 4 - 7;
  AGrid.Cells[0, 0] := 'Table';
  AGrid.Cells[1, 0] := 'Field';
  AGrid.Cells[2, 0] := 'FF Type';
  AGrid.Cells[3, 0] := AWriter.TypeName + ' Type';
  // ---
  k := 1;
  for i := 0 to FDB.Count - 1 do
    inc(k, FDB[i].Count + FDB[i].Indexes.Count);
  AGrid.RowCount := k;

  k := 1;
  for i := 0 to FDB.Count - 1 do begin
    for j := 0 to FDB[i].Count - 1 do begin
      AGrid.Cells[0, k] := FDB[i].Name;
      AGrid.Cells[1, k] := FDB[i][j].Name;
      AGrid.Cells[2, k] := FDB[i][j].TypeName;
      AGrid.Cells[3, k] := AWriter.GetDataTypeName(FDB[i][j]);
      inc(k);
    end;

    for j := 0 to FDB[i].Indexes.Count - 1 do begin
      AGrid.Cells[0, k] := FDB[i].Name + ' (index)';
      AGrid.Cells[1, k] := FDB[i].Indexes[j].Name;
      AGrid.Cells[2, k] := '';
      AGrid.Cells[3, k] := '';
      inc(k);
    end;
  end;
end;

constructor TSourceDBLoader.Create;
begin
  FDB := TSourceDatabase.Create;
  FSQLEngine := TffSqlEngine.Create(nil);
  FServerEngine := TffServerEngine.Create(nil);
  FServerEngine.ConfigDir := GetSpecialPathLocation;
  // --- otherwise the FF server creates temp files anyway
  FServerEngine.BufferManager.ConfigDir := GetSpecialPathLocation;
  FServerEngine.BufferManager.TempStoreSize := 0;
  // ---
  FServerEngine.SQLEngine := FSQLEngine;

  FLoaded := False;
  FConn := nil;
end;

destructor TSourceDBLoader.Destroy;
begin
  if Assigned(FConn) then
    FreeAndNil(FConn);

  FreeAndNil(FSQLEngine);
  FreeAndNil(FServerEngine);
  FreeAndNil(FDB);
  inherited;
end;

function TSourceDBLoader.GetTable(ADBTable: TDBTable): TffTable;
begin
  Result := nil;
  if IsLoaded then begin
    FConn.TRequest.TableName := ADBTable.Name;
    try
      FConn.TRequest.Open;
      if FConn.TRequest.Fields.Count = ADBTable.Count then
        Result := FConn.TRequest;
    except
      on e: Exception do
        Log.Add(lkError, e.Message);
    end;

    if Result = nil then
      Log.Add(ekFFGetDataFailed, 0, ADBTable.Name);
  end;
end;

function TSourceDBLoader.Load(ADirPath: string): Boolean;
var
  list: TStringList;
  table: TffTable;
  i: Integer;
begin
  FLoaded := False;
  FDB.Clear;
  try
    FServerEngine.Stop;
    FServerEngine.Configuration.GeneralInfo^.giServerName := Application.ExeName;
    FServerEngine.Configuration.AddAlias('userdb', ADirPath, False); // ffcl_Path = 219; {count of chars in a directory path (excl final \)}
    FServerEngine.Startup;
    // ----
    if Assigned(FConn) then
      FreeAndNil(FConn);
    FConn := TdmBaseConn.Create(nil);
    table := TffTable.Create(nil);
    list := TStringList.Create;
    try
      FConn.Connect(FServerEngine, 'userdb', ADirPath);

      table.SessionName := FConn.ffSess.SessionName;
      table.DatabaseName := FConn.ffDB.DatabaseName;
      table.ReadOnly := True;
      // ----
      if FConn.IsConnected then begin
        FConn.ffDB.GetTableNames(list);
        for i := 0 to list.Count - 1 do begin
          table.TableName := list[i];
          try
            table.Open;
            FDB.Add(list[i]).AddFields(table.Dictionary);
          except
            on e: Exception do
              Log.Add(lkError, e.Message);
          end;
          table.Close;
        end;
      end;
      // ----
      FLoaded := True;
      Result := True;
    finally
      FreeAndNil(list);
      FreeAndNil(table);
    end;
  except
    on e: Exception do begin
      Log.Add(ekFFConnectFailed, 1, e.Message);
      Result := False;
    end;
  end;
end;

end.

