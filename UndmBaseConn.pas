unit UndmBaseConn;

interface

uses
  SysUtils, ffsreng, Classes, ffdb, ffllbase, ffdbbase, DB;

type
  TdmBaseConn = class(TDataModule)
    ffClient: TffClient;
    ffDB: TffDatabase;
    ffSess: TffSession;
    ffTRequest: TffTable;
    procedure DataModuleDestroy(Sender: TObject);
  public
    procedure Connect(const anEngine: TffServerEngine; const AName: string; const APath: string);
    {-Use this method to connect to an embedded or remote FlashFiler server. }

    procedure Disconnect;
    {-Use this method to disconnect from the FlashFiler server. }

    function IsConnected: Boolean;
    {-Tests the connection with the server.  Returns True if the
      connection is still good otherwise returns False. }

    property Client: TffClient read ffClient;
    {-The client used by this connection. }

    property Database: TffDatabase read ffDB;
    {-The database used by this connection. }

    property Session: TffSession read ffSess;
    {-The session used by this connection. }
    property TRequest: TffTable read ffTRequest;
  end;

implementation

uses UnProcAll, UnLog;

{$R *.dfm}

{ TdmBaseConn }

procedure TdmBaseConn.Connect(const anEngine: TffServerEngine; const AName: string; const APath: string);
var
  index: Integer;
begin
  try
    { Set up the client. }
    with ffClient do begin
      Active := False;
      AutoClientName := True;
      ServerEngine := anEngine;
      Active := True;
    end;

    { Set up the session. }
    with ffSess do begin
      ClientName := ffClient.ClientName;
      AutoSessionName := True;
      Active := True;
    end;

    { Has the alias been defined on the server? }
    if not ffSess.IsAlias(AName) then
      ffSess.AddAlias(AName, APath, False);

    { Set up the database. }
    with ffDB do begin
      SessionName := ffSess.SessionName;
      AutoDatabaseName := True;
      AliasName := AName;
      Open;
    end;

    { Set sessionname and databasename on each table. }
    for index := 0 to ComponentCount - 1 do
      if Components[index] is TffDataSet then begin
        TffDataSet(Components[index]).SessionName := ffSess.SessionName;
        TffDataSet(Components[index]).DatabaseName := ffDB.DatabaseName;
      end;
  except
    on e: Exception do begin
      if ffClient.Active then
        ffClient.Active := False;
      Log.Add(ekFFServerIsBroken, 0, e.Message);
    end;
  end;
end;

procedure TdmBaseConn.Disconnect;
var
  index: Integer;
begin
  try
    if ffDB.Connected then begin
      for index := (ComponentCount - 1) downto 0 do
        if Components[Index] is TffDataSet then
          TffDataSet(Components[Index]).Close;
      ffDB.Connected := False;
    end;
  except
  end;
  ffSess.Close;
  ffClient.Close;
end;

function TdmBaseConn.IsConnected: Boolean;
var
  aTime: TDateTime;
begin
  Result := False;
  try
    { If we can successfully grab the date and time from the server then
      we are still connected. }
    Result := (ffSess.GetServerDateTime(aTime) = 0);
  except
  end;
end;

procedure TdmBaseConn.DataModuleDestroy(Sender: TObject);
begin
  Disconnect;
end;

end.

