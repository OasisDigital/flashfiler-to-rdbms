unit UnConverterSettings;

interface

uses Forms, UnBaseRDBMSWriter;

type
  TRDBMSSettings = class;
  TConverterSettings = class;

  TConverterSettings = class
  private
    FForm: TForm;
    FWriterName: string;
    FSourcePath: string;
    FRDBMS: TRDBMSSettings;
    FRowLimit: Integer;
    FSkipIndexes: Boolean;
    FUseLowerCase: Boolean;
    FUseNonUnicodeTypes: Boolean;
    FUseIdentity: Boolean;

    procedure SetSourcePath(AValue: string);
    procedure SetWriterName(AValue: string);
  public
    constructor Create(AForm: TForm);
    destructor Destroy; override;

    procedure Load;
    procedure LoadRDBMSSettings;
    function GetLowerCase(const ADefault: Boolean): Boolean;
    procedure Save;

    property WriterName: string read FWriterName write SetWriterName;
    property SourcePath: string read FSourcePath write SetSourcePath;
    property RDBMS: TRDBMSSettings read FRDBMS;
    property RowLimit: Integer read FRowLimit write FRowLimit;
    property SkipIndexes: Boolean read FSkipIndexes write FSkipIndexes;
    { to convert all identifiers (table names, column names, index names, etc.) to lower case }
    property UseLowerCase: Boolean read FUseLowerCase write FUseLowerCase;
    property UseNonUnicodeTypes: Boolean read FUseNonUnicodeTypes write FUseNonUnicodeTypes;
    property UseIdentity: Boolean read FUseIdentity write FUseIdentity;
  end;

  TRDBMSSettingsKind = (skStandart, skConnectionString);

  TRDBMSSettings = class
  private
    FKind: TRDBMSSettingsKind;
    FConnectionString_Rec: TCS_Rec;
    FConnectionString: string;
  public
    procedure SetSettings(const AConnectionString: string); overload;
    procedure SetSettings(const AConnectionStringRec: TCS_Rec); overload;

    property Kind: TRDBMSSettingsKind read FKind;
    property ConnectionString: string read FConnectionString;
    property Rec: TCS_Rec read FConnectionString_Rec;
  end;

implementation

uses IniFiles, SysUtils, Dialogs, UnProcAll;

{ TConverterSettings }

constructor TConverterSettings.Create(AForm: TForm);
begin
  FForm := AForm;
  FSourcePath := '';
  FRDBMS := TRDBMSSettings.Create;
  FRowLimit := 0;
  FSkipIndexes := False;
  FUseLowerCase := False;
  FUseNonUnicodeTypes := False;
  FUseIdentity := True;
end;

destructor TConverterSettings.Destroy;
begin
  FreeAndNil(FRDBMS);
  inherited;
end;

function TConverterSettings.GetLowerCase(const ADefault: Boolean): Boolean;
var
  iniF: TiniFile;
begin
  iniF := TIniFile.Create(GetSpecialPathLocation + 'Settings.ini');
  try
    FUseLowerCase := iniF.ReadBool('Converter.Settings', FWriterName + '.UseLowerCase', ADefault);
  finally
    FreeAndNil(iniF);
  end;
  Result := FUseLowerCase;
end;

procedure TConverterSettings.Load;
var
  iniF: TiniFile;
begin
  iniF := TIniFile.Create(GetSpecialPathLocation + 'Settings.ini');

  try
    FWriterName := iniF.ReadString('Converter.Settings', 'WriterName', '');

    FSourcePath := iniF.ReadString('Converter.Settings', 'SourcePath', '');
    if Length(FSourcePath) > 219 then
      FSourcePath := '';

    LoadRDBMSSettings;

    FRowLimit := iniF.ReadInteger('Converter.Settings', 'RowLimit', 0);
    FSkipIndexes := iniF.ReadBool('Converter.Settings', 'SkipIndexes', False);
    FUseLowerCase := GetLowerCase(False);
    FUseNonUnicodeTypes := iniF.ReadBool('Converter.Settings', 'UseNonUnicodeTypes', False);
    FUseIdentity := iniF.ReadBool('Converter.Settings', 'UseIdentity', True);

    if Assigned(FForm) then begin
      FForm.Left := iniF.ReadInteger('Converter.Settings', 'Form.Left', FForm.Left);
      FForm.Top := iniF.ReadInteger('Converter.Settings', 'Form.Top', FForm.Top);
      FForm.Width := iniF.ReadInteger('Converter.Settings', 'Form.Width', FForm.Width);
      FForm.Height := iniF.ReadInteger('Converter.Settings', 'Form.Heigth', FForm.Height);
    end;
  finally
    FreeAndNil(iniF);
  end;
end;

procedure TConverterSettings.LoadRDBMSSettings;
var
  iniF: TiniFile;
  rec: TCS_Rec;
begin
  iniF := TIniFile.Create(GetSpecialPathLocation + 'Settings.ini');

  try
    if iniF.ReadInteger('Converter.Settings', FWriterName+'.RDBMS.Kind', Integer(skConnectionString)) = Integer(skConnectionString) then
      FRDBMS.SetSettings(iniF.ReadString('Converter.Settings', FWriterName+'.RDBMS.ConnectionString', ''))
    else begin
      rec.Host := iniF.ReadString('Converter.Settings', FWriterName+'.RDBMS.Host', '');
      rec.Port := iniF.ReadString('Converter.Settings', FWriterName+'.RDBMS.Port', '');
      rec.DB := iniF.ReadString('Converter.Settings', FWriterName+'.RDBMS.DB', '');
      rec.UserName := iniF.ReadString('Converter.Settings', FWriterName+'.RDBMS.UserName', '');
      rec.Password := iniF.ReadString('Converter.Settings', FWriterName+'.RDBMS.Password', '');
      rec.Trusted := iniF.ReadBool('Converter.Settings', FWriterName+'.RDBMS.Trusted', False);

      FRDBMS.SetSettings(rec)
    end;
  finally
    FreeAndNil(iniF);
  end;
end;

procedure TConverterSettings.Save;
var
  iniF: TiniFile;
begin
  iniF := TIniFile.Create(GetSpecialPathLocation + 'Settings.ini');

  try
    iniF.WriteString('Converter.Settings', 'WriterName', FWriterName);
    iniF.WriteString('Converter.Settings', 'SourcePath', FSourcePath);

    iniF.WriteInteger('Converter.Settings', FWriterName+'.RDBMS.Kind', Integer(FRDBMS.Kind));
    if FRDBMS.Kind = skConnectionString then
      iniF.WriteString('Converter.Settings', FWriterName+'.RDBMS.ConnectionString', FRDBMS.ConnectionString)
    else begin
      iniF.WriteString('Converter.Settings', FWriterName+'.RDBMS.Host', FRDBMS.Rec.Host);
      iniF.WriteString('Converter.Settings', FWriterName+'.RDBMS.Port', FRDBMS.Rec.Port);
      iniF.WriteString('Converter.Settings', FWriterName+'.RDBMS.DB', FRDBMS.Rec.DB);
      iniF.WriteString('Converter.Settings', FWriterName+'.RDBMS.UserName', FRDBMS.Rec.UserName);
      iniF.WriteString('Converter.Settings', FWriterName+'.RDBMS.Password', FRDBMS.Rec.Password);
      iniF.WriteBool('Converter.Settings', FWriterName+'.RDBMS.Trusted', FRDBMS.Rec.Trusted);
    end;

    iniF.WriteInteger('Converter.Settings', 'RowLimit', FRowLimit);
    iniF.WriteBool('Converter.Settings', 'SkipIndexes', FSkipIndexes);
    iniF.WriteBool('Converter.Settings', FWriterName+'.UseLowerCase', FUseLowerCase);
    iniF.WriteBool('Converter.Settings', 'UseNonUnicodeTypes', FUseNonUnicodeTypes);
    iniF.WriteBool('Converter.Settings', 'UseIdentity', FUseIdentity);

    if Assigned(FForm) then begin
      iniF.WriteInteger('Converter.Settings', 'Form.Left', FForm.Left);
      iniF.WriteInteger('Converter.Settings', 'Form.Top', FForm.Top);
      iniF.WriteInteger('Converter.Settings', 'Form.Width', FForm.Width);
      iniF.WriteInteger('Converter.Settings', 'Form.Heigth', FForm.Height);
    end;
  finally
    FreeAndNil(iniF);
  end;
end;

procedure TConverterSettings.SetSourcePath(AValue: string);
begin
  if Length(AValue) <= 219 then
    FSourcePath := AValue
  else
    MessageDlg('Count of chars in a directory path must be less than 219.', mtError, [mbOk], 0)
end;

procedure TConverterSettings.SetWriterName(AValue: string);
begin
  FWriterName := AValue;
  LoadRDBMSSettings;
end;

{ TRDBMSSettings }

procedure TRDBMSSettings.SetSettings(const AConnectionString: string);
begin
  FKind := skConnectionString;
  FConnectionString := AConnectionString;
end;

procedure TRDBMSSettings.SetSettings(const AConnectionStringRec: TCS_Rec);
begin
  FKind := skStandart;
  FConnectionString_Rec := AConnectionStringRec;
end;

end.

