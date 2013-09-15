unit UnMain;

interface

{$WARN UNIT_PLATFORM OFF}

uses
  Windows, Forms, UnSourceDBLoader, UnConverterSettings, UnBaseRDBMSWriter,
  ShellCtrls, Spin, XPMan, Grids, Buttons, ExtCtrls, ComCtrls, StdCtrls, Dialogs,
  Controls, Classes;

type
  TFF_RDBMS_Converter = class;

  TStepKind = (stkIntroduction,
    stkSelectFF,
    stkReviewFF,
    stkSelectSQLServer,
    stkConfirm,
    stkProgress,
    stkResult);

  TfmFF_RDBMS_Converter = class(TForm)
    pOkCancel: TPanel;
    bPrior: TButton;
    bNext: TButton;
    XPManifest1: TXPManifest;
    pc: TPageControl;
    tsIntroduction: TTabSheet;
    tsSelectFF: TTabSheet;
    tshReviewFF: TTabSheet;
    tshSelectMSSQL: TTabSheet;
    tvFF: TShellTreeView;
    Panel3: TPanel;
    Label3: TLabel;
    Panel4: TPanel;
    Label4: TLabel;
    sgGrid: TStringGrid;
    Label2: TLabel;
    Panel5: TPanel;
    Label9: TLabel;
    Panel7: TPanel;
    rbStandard: TRadioButton;
    chbRDBMS_Trusted: TCheckBox;
    rbConnectionString: TRadioButton;
    edConnectionString: TEdit;
    sbConnectionStringEdit: TSpeedButton;
    tshConfirm: TTabSheet;
    Panel8: TPanel;
    Label5: TLabel;
    Label6: TLabel;
    seRowLimit: TSpinEdit;
    Label7: TLabel;
    Label8: TLabel;
    edLogFile: TLabeledEdit;
    edRDBMS_Host: TLabeledEdit;
    edRDBMS_DB: TLabeledEdit;
    edRDBMS_UserName: TLabeledEdit;
    edRDBMS_Password: TLabeledEdit;
    SpeedButton2: TSpeedButton;
    OpenDialog1: TOpenDialog;
    tshProgress: TTabSheet;
    tshResult: TTabSheet;
    Panel1: TPanel;
    pbValue0: TProgressBar;
    pbValue1: TProgressBar;
    mLog: TMemo;
    Label1: TLabel;
    bCancel: TButton;
    lRowsCount: TLabel;
    Label10: TLabel;
    cbRDBMSType: TComboBox;
    edRDBMS_Port: TLabeledEdit;
    cbSkipIndexes: TCheckBox;
    Label11: TLabel;
    Edit1: TEdit;
    cbUseNonUnicodeTypes: TCheckBox;
    mResult: TMemo;
    Label12: TLabel;
    Panel2: TPanel;
    lResultLeft: TLabel;
    pResult: TPanel;
    lResultN: TLabel;
    lResultV: TLabel;
    cbUseLowerCase: TCheckBox;
    cbUseIdentity: TCheckBox;
    procedure bNextClick(Sender: TObject);
    procedure bPriorClick(Sender: TObject);
    procedure sbConnectionStringEditClick(Sender: TObject);
    procedure chbRDBMS_TrustedClick(Sender: TObject);
    procedure rbStandardClick(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbUseNonUnicodeTypesClick(Sender: TObject);
  protected
    Converter: TFF_RDBMS_Converter;

    procedure SettingsBind(const AStep: TStepKind);
    procedure SettingsUpdate(const AStep: TStepKind);

    procedure PageControlUpdate;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TFF_RDBMS_Converter = class
  private
    FFM: TfmFF_RDBMS_Converter;
    FLoader: TSourceDBLoader;
    FWriter: TBaseRDBMSWriter;
    FSettings: TConverterSettings;

    function GetIsLoadedSchema: Boolean;
  protected
    procedure ProgressStart;
    procedure ProgressMain(const AValue: Integer);
    procedure ProgressDetail(const AValue: Integer);
    procedure ProgressStop;
  public
    constructor Create(AForm: TfmFF_RDBMS_Converter);
    destructor Destroy; override;

    procedure Clear;
    function LoadSchema(const ABind: Boolean = False): Boolean;
    function InitWriter(const AWriterName: string): Boolean;

    procedure Execute;
    procedure Cancel;

    property Loader: TSourceDBLoader read FLoader;
    property Writer: TBaseRDBMSWriter read FWriter;
    property Settings: TConverterSettings read FSettings;
    property IsLoadedSchema: Boolean read GetIsLoadedSchema;
  end;

var
  fmFF_RDBMS_Converter: TfmFF_RDBMS_Converter;

implementation

uses
  ADODB, ffdb, SysUtils, Math, StrUtils, UnSourceDatabase, UnLog, UnMSSQLWriter,
  UnPGSQLWriter;

{$R *.dfm}

{ TFF_RDBMS_Converter }

procedure TFF_RDBMS_Converter.Clear;
begin
  if Assigned(FLoader) then
    FreeAndNil(FLoader);
  if Assigned(Writer) then
    Writer.Disconnect;
end;

constructor TFF_RDBMS_Converter.Create(AForm: TfmFF_RDBMS_Converter);
begin
  FFM := AForm;
  FSettings := TConverterSettings.Create(AForm);
  FLoader := nil;
  FWriter := nil;

  FSettings.Load;
end;

destructor TFF_RDBMS_Converter.Destroy;
begin
  FSettings.Save;
  Clear;

  FreeAndNil(FSettings);
  inherited;
end;

procedure TFF_RDBMS_Converter.Execute;
var
  cs, n, v: string;

  procedure CorrectIndexNames(ADB: TSourceDatabase);
  var
    i, j: Integer;
    list: TStringList;
  begin
    list := TStringList.Create;

    try
      for i := 0 to ADB.Count - 1 do
        list.Add(ADB[i].Name);

      for i := 0 to ADB.Count - 1 do
        for j := 0 to ADB[i].Indexes.Count - 1 do
          if list.IndexOf(ADB[i].Indexes[j].Name) = -1 then
            list.Add(ADB[i].Indexes[j].Name)
          else begin
            ADB[i].Indexes[j].GenerateNewName(list);
            list.Add(ADB[i].Indexes[j].Name);
          end;
    finally
      FreeAndNil(list);
    end;
  end;
begin
  Log.Control := FFM.mLog;
  Log.Clear;
  ProgressStart;
  // ----
  Log.AddSeparator('Common information');
  Log.Add(lkInfo, 'Directory (FileFiler) = ' + Settings.SourcePath);
  if Settings.RDBMS.Kind = skStandart then
    cs := Writer.GetConnectionString(Settings.RDBMS.Rec)
  else
    cs := Settings.RDBMS.ConnectionString;
  Log.Add(lkInfo, Format('Connection string (%s) =  %s', [Writer.TypeFullName, cs]));
  if not (IsLoadedSchema or LoadSchema) or
     not (Writer.IsConnected or Writer.Connect(cs)) then begin
    Log.Control := nil;
    Exit
  end;
  Log.Add(lkSuccess, 'Connection has been done.');
  Log.AddEmptyLine;
  // -----
  Log.AddSeparator('Conversion of tables');
  try
    FLoader.SourceDB.UseLowerCase := Writer.UseLowerCase;
    Writer.Statistic.Clear;
    Writer.SaveScriptSchemaOfTables(FLoader.SourceDB);
    Writer.DoCreateSchemaOfTables(FLoader.SourceDB);
    // ----
    if not Writer.Canceled and (Writer.Statistic.Errors = 0) or
      (MessageDlg('Do you want to proceed?', mtError, [mbYes, mbCancel], 0) = mrYes) then begin
      Writer.DoTransferData(FLoader.SourceDB, FLoader.GetTable, Settings.RowLimit);
      // ---
      if not Writer.Canceled then begin
        Writer.DoCreatePrimaryKeys(FLoader.SourceDB);
        CorrectIndexNames(FLoader.SourceDB);
        Writer.SaveScriptIndexesOfTables(FLoader.SourceDB);
        if not Settings.SkipIndexes then
          Writer.DoCreateIndexesOfTables(FLoader.SourceDB);
      end;
    end;
    Log.AddSeparator;
    // ---
    FFM.lResultLeft.Caption := 'Results:' + #13#10 + #13#10 + #13#10 + #13#10 + 'See log for details.';

    Writer.Statistic.DoLog(n, v);
    FFM.lResultN.Caption := n;
    FFM.lResultV.Caption := v;
    FFM.pResult.Left := (FFM.pResult.Parent.Width div 2) - ((FFM.lResultN.Width + FFM.lResultV.Width) div 2);
  finally
    Log.Control := nil;
    ProgressStop;
  end;
end;

function TFF_RDBMS_Converter.GetIsLoadedSchema: Boolean;
begin
  Result := Assigned(FLoader);
end;

function TFF_RDBMS_Converter.LoadSchema(const ABind: Boolean = False): Boolean;
var
  k, i: Integer;
  q: TffTable;
begin
  Clear;

  FLoader := TSourceDBLoader.Create;
  Result := FLoader.Load(Settings.SourcePath);
  if ABind then begin
    FLoader.Bind(FFM.sgGrid, Writer);
    k := 0;
    if Result then
      for i := 0 to FLoader.SourceDB.Count - 1 do begin
        q := FLoader.GetTable(FLoader.SourceDB[i]);
        if Assigned(q) then
          inc(k, q.RecordCount);
      end;
    FFM.lRowsCount.Caption := 'Count of rows: ' + IntToStr(k);
  end;
end;

procedure TFF_RDBMS_Converter.ProgressStart;
begin
  ProgressMain(0);
  ProgressDetail(0);
end;

procedure TFF_RDBMS_Converter.ProgressStop;
begin
  ProgressMain(0);
  ProgressDetail(0);
end;

procedure TFF_RDBMS_Converter.ProgressMain(const AValue: Integer);
begin
  FFM.pbValue0.Position := AValue;
  if AValue = 0 then
    ProgressDetail(0);
end;

procedure TFF_RDBMS_Converter.ProgressDetail(const AValue: Integer);
begin
  FFM.pbValue1.Position := AValue;
end;

constructor TfmFF_RDBMS_Converter.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;
  Converter := TFF_RDBMS_Converter.Create(Self);

  for i := 0 to pc.PageCount - 1 do
    pc.Pages[i].TabVisible := False;

  pc.ActivePageIndex := 0;
  PageControlUpdate;
end;

destructor TfmFF_RDBMS_Converter.Destroy;
begin
  FreeAndNil(Converter);
  inherited;
end;

procedure TfmFF_RDBMS_Converter.bNextClick(Sender: TObject);
var
  step: TStepKind;
  cur: TCursor;
  cs: string;

  procedure NextPage;
  begin
    pc.ActivePageIndex := pc.ActivePageIndex + 1;
    SettingsBind(TStepKind(pc.ActivePageIndex));
    PageControlUpdate;
  end;
begin
  step := TStepKind(pc.ActivePageIndex);

  cur := Screen.Cursor;
  Screen.Cursor := crHourGlass;

  case step of
    stkIntroduction:
      begin
        SettingsUpdate(step);
        if Converter.InitWriter(Converter.Settings.WriterName) then
          NextPage;
      end;
    stkSelectFF:
      begin
        SettingsUpdate(step);
        if not Converter.LoadSchema(True) then
          MessageDlg('Files could not be opened. Files may be in use by another process.', mtWarning, [mbOK], 0)
        else
          NextPage;
      end;
    stkReviewFF:
      begin
        SettingsUpdate(step);
        NextPage;
      end;
    stkSelectSQLServer:
      begin
        SettingsUpdate(step);
        if Converter.Settings.RDBMS.Kind = skStandart then
          cs := Converter.Writer.GetConnectionString(Converter.Settings.RDBMS.Rec)
        else
          cs := Converter.Settings.RDBMS.ConnectionString;
        if not Converter.Writer.Connect(cs) then
          MessageDlg(Converter.Writer.TypeFullName + ' does not exist or access denied.', mtWarning, [mbOK], 0)
        else
          NextPage;
      end;
    stkConfirm:
      begin
        SettingsUpdate(stkConfirm);

        NextPage;
        Converter.Execute;

        mResult.Lines.Assign(mLog.Lines);
        NextPage;
      end;
    stkResult: Close;
  end;

  Screen.Cursor := cur;
end;

procedure TfmFF_RDBMS_Converter.bPriorClick(Sender: TObject);
begin
  if pc.ActivePageIndex > 0 then begin
    pc.ActivePageIndex := pc.ActivePageIndex - 1;
    PageControlUpdate;
  end;
end;

procedure TfmFF_RDBMS_Converter.sbConnectionStringEditClick(Sender: TObject);
begin
  edConnectionString.Text := PromptDataSource(Self.Handle, edConnectionString.Text)
end;

procedure TfmFF_RDBMS_Converter.chbRDBMS_TrustedClick(Sender: TObject);
begin
  edRDBMS_UserName.Enabled := not chbRDBMS_Trusted.Checked;
  edRDBMS_Password.Enabled := not chbRDBMS_Trusted.Checked;
end;

procedure TfmFF_RDBMS_Converter.rbStandardClick(Sender: TObject);
begin
  edRDBMS_DB.Enabled := rbStandard.Checked;
  edRDBMS_Host.Enabled := rbStandard.Checked;
  edRDBMS_Port.Enabled := rbStandard.Checked;
  chbRDBMS_Trusted.Enabled := rbStandard.Checked;
  edRDBMS_UserName.Enabled := rbStandard.Checked and not chbRDBMS_Trusted.Checked;
  edRDBMS_Password.Enabled := rbStandard.Checked and not chbRDBMS_Trusted.Checked;
  edConnectionString.Enabled := not rbStandard.Checked;
  sbConnectionStringEdit.Enabled := not rbStandard.Checked;
end;

procedure TfmFF_RDBMS_Converter.SpeedButton2Click(Sender: TObject);
begin
  OpenDialog1.FileName := edLogFile.Text;
  if OpenDialog1.Execute then
    edLogFile.Text := OpenDialog1.FileName;
end;

procedure TfmFF_RDBMS_Converter.bCancelClick(Sender: TObject);
begin
  Converter.Cancel;
  Close;
end;

procedure TfmFF_RDBMS_Converter.PageControlUpdate;
var
  step: TStepKind;
begin
  step := TStepKind(pc.ActivePageIndex);

  bCancel.Enabled := step <> stkResult;
  bPrior.Enabled := not (step in [stkIntroduction, stkProgress, stkResult]);
  bNext.Enabled := not (step in [stkProgress]);

  bCancel.Caption := IfThen(step <> stkResult, 'Cancel', '');
  bPrior.Caption := IfThen(not (step in [stkIntroduction, stkProgress, stkResult]), 'Prior', '');
  bNext.Caption := IfThen(step = stkProgress, '', IfThen(step = stkResult, 'Done', 'Next'));
end;

function TFF_RDBMS_Converter.InitWriter(const AWriterName: string): Boolean;
begin
  if Assigned(FWriter) then
    FreeAndNil(FWriter);

  if AWriterName = TPGSQLWriter.ClassName then
    FWriter := TPGSQLWriter.Create
  else
    FWriter := TMSSQLWriter.Create;

  Result := Assigned(FWriter);
  if Result then begin
    FWriter.ProgressMainEvent := ProgressMain;
    FWriter.ProgressDetailEvent := ProgressDetail;
    if FWriter.CanUseNonUnicodeTypes then
      FWriter.UseNonUnicodeTypes := Settings.UseNonUnicodeTypes;
    FWriter.UseLowerCase := Settings.GetLowerCase(FWriter.UseLowerCase);
    if FWriter.CanUseIdentity then
      FWriter.UseIdentity := Settings.UseIdentity;
  end;
end;

procedure TfmFF_RDBMS_Converter.FormShow(Sender: TObject);
begin
  cbRDBMSType.Items.Clear;
  cbRDBMSType.Items.Add(TMSSQLWriter.GetTypeFullName);
  cbRDBMSType.Items.Add(TPGSQLWriter.GetTypeFullName);
  cbRDBMSType.ItemIndex := 0;
  SettingsBind(stkIntroduction);
end;

procedure TfmFF_RDBMS_Converter.SettingsBind(const AStep: TStepKind);
  procedure RDBMS_ComponentsUpdate;
  var
    i: Integer;
  begin
    edRDBMS_Host.Text := '';
    edRDBMS_Port.Text := '';
    edRDBMS_DB.Text := '';
    edRDBMS_Password.Text := '';
    edRDBMS_UserName.Text := '';
    chbRDBMS_Trusted.Checked := False;
    edConnectionString.Text := '';
    Label9.Caption := Format('Connect (%s)', [Converter.Writer.TypeFullName]);
    // ---
    i := 4;
    edRDBMS_Host.Visible := (Converter.Writer.CS_Flags and CS_Host <> 0);
    if edRDBMS_Host.Visible then begin
      edRDBMS_Host.Top := i;
      inc(i, edRDBMS_Host.Height);
    end;

    edRDBMS_Port.Visible := (Converter.Writer.CS_Flags and CS_Port <> 0);
    if edRDBMS_Port.Visible then begin
      edRDBMS_Port.Top := i;
      inc(i, edRDBMS_Port.Height);
    end;

    edRDBMS_DB.Visible := (Converter.Writer.CS_Flags and CS_DB <> 0);
    if edRDBMS_DB.Visible then
      edRDBMS_DB.Top := i;
    // ---
    i := 4;
    edRDBMS_UserName.Visible := (Converter.Writer.CS_Flags and CS_UserName <> 0);
    if edRDBMS_UserName.Visible then begin
      edRDBMS_UserName.Top := i;
      inc(i, edRDBMS_UserName.Height);
    end;

    edRDBMS_Password.Visible := (Converter.Writer.CS_Flags and CS_Password <> 0);
    if edRDBMS_Password.Visible then begin
      edRDBMS_Password.Top := i;
      inc(i, edRDBMS_Password.Height);
    end;

    chbRDBMS_Trusted.Visible := (Converter.Writer.CS_Flags and CS_Trusted <> 0);
    if chbRDBMS_Trusted.Visible then
      chbRDBMS_Trusted.Top := i;
  end;
begin
  case AStep of
    stkIntroduction:
      cbRDBMSType.ItemIndex := IfThen(Converter.Settings.WriterName = TPGSQLWriter.ClassName, 1, 0);
    stkSelectFF:
      if DirectoryExists(Converter.Settings.SourcePath) then
        tvFF.Path := Converter.Settings.SourcePath;
    stkReviewFF:
      begin
        cbUseNonUnicodeTypes.Visible := Converter.Writer.CanUseNonUnicodeTypes;
        cbUseNonUnicodeTypes.Checked := Converter.Settings.UseNonUnicodeTypes;
        cbUseIdentity.Visible := Converter.Writer.CanUseIdentity;
        cbUseIdentity.Checked := Converter.Settings.UseIdentity;
      end;
    stkSelectSQLServer:
      begin
        RDBMS_ComponentsUpdate;

        rbConnectionString.Checked := Converter.Settings.RDBMS.Kind <> skStandart;
        if rbConnectionString.Checked then
          edConnectionString.Text := Converter.Settings.RDBMS.ConnectionString
        else begin
          edRDBMS_Host.Text := Converter.Settings.RDBMS.Rec.Host;
          edRDBMS_Port.Text := Converter.Settings.RDBMS.Rec.Port;
          edRDBMS_DB.Text := Converter.Settings.RDBMS.Rec.DB;
          chbRDBMS_Trusted.Checked := Converter.Settings.RDBMS.Rec.Trusted;
          chbRDBMS_Trusted.OnClick(chbRDBMS_Trusted);
          if not chbRDBMS_Trusted.Checked then begin
            edRDBMS_UserName.Text := Converter.Settings.RDBMS.Rec.UserName;
            edRDBMS_Password.Text := Converter.Settings.RDBMS.Rec.Password;
          end;
        end;
      end;
    stkConfirm:
      begin
        edLogFile.Text := Log.LogFile;
        seRowLimit.Value := Converter.Settings.RowLimit;
        cbSkipIndexes.Checked := Converter.Settings.SkipIndexes;
        cbUseLowerCase.Checked := Converter.Settings.UseLowerCase;
      end;
  end;
end;

procedure TfmFF_RDBMS_Converter.SettingsUpdate(const AStep: TStepKind);
var
  Rec: TCS_Rec;
begin
  case AStep of
    stkIntroduction:
      Converter.Settings.WriterName := IfThen(cbRDBMSType.ItemIndex = 1, TPGSQLWriter.ClassName, TMSSQLWriter.ClassName);
    stkSelectFF:
      Converter.Settings.SourcePath := tvFF.Path + '\';
    stkReviewFF:
      begin
        if Converter.Writer.CanUseNonUnicodeTypes then begin
          Converter.Settings.UseNonUnicodeTypes := cbUseNonUnicodeTypes.Checked;
          Converter.Writer.UseNonUnicodeTypes := cbUseNonUnicodeTypes.Checked;
        end;
        if Converter.Writer.CanUseIdentity then begin
          Converter.Settings.UseIdentity := cbUseIdentity.Checked;
          Converter.Writer.UseIdentity := cbUseIdentity.Checked;
        end;
      end;
    stkSelectSQLServer:
      if rbConnectionString.Checked then
        Converter.Settings.RDBMS.SetSettings(edConnectionString.Text)
      else begin
        rec.Host := edRDBMS_Host.Text;
        rec.Port := edRDBMS_Port.Text;
        rec.DB := edRDBMS_DB.Text;
        rec.Trusted := chbRDBMS_Trusted.Checked;
        if not rec.Trusted then begin
          rec.UserName := edRDBMS_UserName.Text;
          rec.Password := edRDBMS_Password.Text;
        end;
        Converter.Settings.RDBMS.SetSettings(rec);
      end;
    stkConfirm:
      begin
        Log.LogFile := edLogFile.Text;
        Converter.Settings.RowLimit := seRowLimit.Value;
        Converter.Settings.SkipIndexes := cbSkipIndexes.Checked;
        Converter.Settings.UseLowerCase := cbUseLowerCase.Checked;
        Converter.Writer.UseLowerCase := cbUseLowerCase.Checked;
      end;
  end;
end;

procedure TfmFF_RDBMS_Converter.cbUseNonUnicodeTypesClick(Sender: TObject);
begin
  Converter.Writer.UseNonUnicodeTypes := cbUseNonUnicodeTypes.Checked;
  Converter.Loader.Bind(sgGrid, Converter.Writer);
end;

procedure TFF_RDBMS_Converter.Cancel;
begin
  if Assigned(Writer) then
    Writer.Cancel;
end;

end.

