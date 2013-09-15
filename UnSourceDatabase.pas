unit UnSourceDatabase;

interface

uses Classes, fflldict, ffllbase, ADODB, DB;

type
  TSourceDatabase = class;
  TDBTable = class;
  TDBField = class;
  TDBIndexes = class;
  TDBIndex = class;

  TSourceDatabase = class
  private
    FItems: TList;
    FUseLowerCase: Boolean;

    function GetCount: Integer;
    function GetTable(AIndex: Integer): TDBTable;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function Add(AName: string): TDBTable;

    property UseLowerCase: Boolean read FUseLowerCase write FUseLowerCase;
    property Count: Integer read GetCount;
    property Tables[Index: Integer]: TDBTable read GetTable; default;
  end;

  TDBTable = class
  private
    FOwner: TSourceDatabase;
    FName: string;
    FFields: TList;
    FIndexes: TDBIndexes;
    FPrimaryKey: TList;

    function GetCount: Integer;
    function GetField(AIndex: Integer): TDBField;
    function GetPrimaryKeyCount: Integer;
    function GetPrimaryKey(AIndex: Integer): TDBField;
    function GetName: string;
  public
    constructor Create(AOwner: TSourceDatabase; const AName: string);
    destructor Destroy; override;

    procedure Clear;
    procedure AddFields(ADictionary: TffDataDictionary);

    property DB: TSourceDatabase read FOwner;
    property Name: string read GetName;
    property Count: Integer read GetCount;
    property Fields[Index: Integer]: TDBField read GetField; default;
    property Indexes: TDBIndexes read FIndexes;
    property PrimaryKeyCount: Integer read GetPrimaryKeyCount;
    property PrimaryKey[Index: Integer]: TDBField read GetPrimaryKey;
  end;

  TDBField = class
  private
    FOwner: TDBTable;
    FName: string;
    FType: TffFieldType;
    FUnits: Integer;
    FDecimals: Integer;
    FHasDefault: Boolean;
    FDefault: string;

    function GetTypeName_FF: string;
    function GetName: string;
  public
    constructor Create(AOwner: TDBTable; const AName: string; const AType: TffFieldType);

    property Table: TDBTable read FOwner;
    property Name: string read GetName;
    property Type_: TffFieldType read FType;
    property TypeName: string read GetTypeName_FF;
    property Units: Integer read FUnits;
    property Decimals: Integer read FDecimals;
    property HasDefault: Boolean read FHasDefault;
    property Default_: string read FDefault;
  end;

  TDBIndexes = class
  private
    FOwner: TDBTable;
    FItems: TList;

    function GetCount: Integer;
    function GetIndex(AIndex: Integer): TDBIndex;
  public
    constructor Create(AOwner: TDBTable);
    destructor Destroy; override;

    procedure Clear;
    procedure LoadFromDict(ADictionary: TffDataDictionary);

    property Table: TDBTable read FOwner;
    property Count: Integer read GetCount;
    property Indexes[AIndex: Integer]: TDBIndex read GetIndex; default;
  end;

  TDBIndex = class
  private
    FOwner: TDBIndexes;
    FName: string;
    FUnique: Boolean;
    FFields: TList;

    function GetCount: Integer;
    function GetField(AIndex: Integer): TDBField;
    function GetName: string;
  public
    constructor Create(AOwner: TDBIndexes; const AName: string);
    destructor Destroy; override;

    procedure GenerateNewName(AList: TStringList);

    property Indexes: TDBIndexes read FOwner;
    property Name: string read GetName;
    property Unique: Boolean read FUnique;
    property Count: Integer read GetCount;
    property Fields[AIndex: Integer]: TDBField read GetField; default;
  end;

implementation

uses SysUtils, Math, ffutil, UnLog;

{ TSourceDatabase }

function TSourceDatabase.Add(AName: string): TDBTable;
begin
  Result := Tables[FItems.Add(TDBTable.Create(Self, AName))];
end;

procedure TSourceDatabase.Clear;
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    TDBTable(FItems[i]).Free;
  FItems.Clear;
end;

constructor TSourceDatabase.Create;
begin
  FItems := TList.Create;
end;

destructor TSourceDatabase.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TSourceDatabase.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TSourceDatabase.GetTable(AIndex: Integer): TDBTable;
begin
  if InRange(AIndex, 0, Count - 1) then
    Result := TDBTable(FItems[AIndex])
  else
    Result := nil;
end;

{ TDBTable }

procedure TDBTable.AddFields(ADictionary: TffDataDictionary);
var
  i, j: Integer;
  field: TDBField;
begin
  Clear;
  for i := 0 to ADictionary.FieldCount - 1 do begin
    field := Fields[FFields.Add(TDBField.Create(Self, ADictionary.FieldName[i], ADictionary.FieldType[i]))];
    field.FUnits := ADictionary.FieldUnits[i];
    field.FDecimals := ADictionary.FieldDecPl[i];
    field.FHasDefault := Assigned(ADictionary.FieldVCheck[i]) and ADictionary.FieldVCheck[i].vdHasDefVal;
    if field.FHasDefault then
      field.FDefault := FFVCheckValToString(ADictionary.FieldVCheck[i].vdDefVal, field.Type_);
  end;
  FIndexes.LoadFromDict(ADictionary);
  // ---
  FPrimaryKey.Clear;
  for i:=0 to Count-1 do begin
    for j:=0 to Indexes.Count-1 do
    if Indexes[j].Unique and (Indexes[j].FFields.IndexOf(Fields[i]) <> -1) then begin
      FPrimaryKey.Assign(Indexes[j].FFields);
      Break;
    end;
    if FPrimaryKey.Count <> 0 then
      Break;
  end;
end;

procedure TDBTable.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TDBField(FFields[i]).Free;
  FFields.Clear;
  FPrimaryKey.Clear;
end;

constructor TDBTable.Create(AOwner: TSourceDatabase; const AName: string);
begin
  FOwner := AOwner;
  FName := AName;
  FFields := TList.Create;
  FIndexes := TDBIndexes.Create(Self);
  FPrimaryKey := TList.Create;
end;

destructor TDBTable.Destroy;
begin
  Clear;
  FreeAndNil(FPrimaryKey);
  FreeAndNil(FIndexes);
  FreeAndNil(FFields);
  inherited;
end;

function TDBTable.GetCount: Integer;
begin
  Result := FFields.Count;
end;

function TDBTable.GetField(AIndex: Integer): TDBField;
begin
  if InRange(AIndex, 0, Count - 1) then
    Result := TDBField(FFields[AIndex])
  else
    Result := nil;
end;

function TDBTable.GetName: string;
begin
  if FOwner.UseLowerCase then
    Result := LowerCase(FName)
  else
    Result := FName;
end;

function TDBTable.GetPrimaryKey(AIndex: Integer): TDBField;
begin
  if InRange(AIndex, 0, PrimaryKeyCount - 1) then
    Result := TDBField(FPrimaryKey[AIndex])
  else
    Result := nil;
end;

function TDBTable.GetPrimaryKeyCount: Integer;
begin
  Result := FPrimaryKey.Count;
end;

{ TDBIndexes }

procedure TDBIndexes.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TDBIndex(FItems[i]).Free;
  FItems.Clear;
end;

constructor TDBIndexes.Create(AOwner: TDBTable);
begin
  FOwner := AOwner;
  FItems := TList.Create;
end;

destructor TDBIndexes.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TDBIndexes.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TDBIndexes.GetIndex(AIndex: Integer): TDBIndex;
begin
  if InRange(AIndex, 0, Count - 1) then
    Result := TDBIndex(FItems[AIndex])
  else
    Result := nil;
end;

procedure TDBIndexes.LoadFromDict(ADictionary: TffDataDictionary);
var
  i, j: Integer;
  index: TDBIndex;
begin
  Clear;
  for i := 0 to ADictionary.IndexCount - 1 do
    if ADictionary.IndexDescriptor[i]^.idCount > 0 then begin
      index := Indexes[FItems.Add(TDBIndex.Create(Self, ADictionary.IndexDescriptor[i]^.idName))];
      index.FUnique := not ADictionary.IndexDescriptor[i]^.idDups;
      for j := 0 to ADictionary.IndexDescriptor[i]^.idCount - 1 do
        index.FFields.Add(Table[ADictionary.IndexDescriptor[i]^.idFields[j]]);
    end;
end;

{ TDBIndex }

constructor TDBIndex.Create(AOwner: TDBIndexes; const AName: string);
begin
  FOwner := AOwner;
  FName := AName;
  FUnique := False;
  FFields := TList.Create;
end;

destructor TDBIndex.Destroy;
begin
  FreeAndNil(FFields);
  inherited;
end;

procedure TDBIndex.GenerateNewName(AList: TStringList);
var
  k: Integer;
  s: string;
begin
  k := 0;
  s := Name;
  while (AList.IndexOf(s) <> -1) do begin
    s := Format('%s%d', [Name, k]);
    inc(k);
  end;
  Log.Add(lkInfo, Format('Change index name (%s) from %s to %s', [FOwner.Table.Name, Name, s]));
  FName := s;
end;

function TDBIndex.GetCount: Integer;
begin
  Result := FFields.Count;
end;

function TDBIndex.GetField(AIndex: Integer): TDBField;
begin
  if InRange(AIndex, 0, Count - 1) then
    Result := TDBField(FFields[AIndex])
  else
    Result := nil;
end;

function TDBIndex.GetName: string;
begin
  if FOwner.FOwner.FOwner.UseLowerCase then
    Result := LowerCase(FName)
  else
    Result := FName;
end;

{ TDBField }

constructor TDBField.Create(AOwner: TDBTable; const AName: string;
  const AType: TffFieldType);
begin
  FOwner := AOwner;
  FName := AName;
  FType := AType;
  FUnits := 0;
  FDecimals := 0;
  FHasDefault := False;
  FDefault := '';
end;

function TDBField.GetName: string;
begin
  if FOwner.FOwner.UseLowerCase then
    Result := LowerCase(FName)
  else
    Result := FName;
end;

function TDBField.GetTypeName_FF: string;
begin
  Result := ffllbase.FieldDataTypes[Type_];
end;

end.

