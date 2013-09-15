unit UnProcAll;

interface

uses DB, UnSourceDatabase, ffdb;

type
  TGetSourceTable = function(ADBTable: TDBTable): TffTable of object;
  TProgressEvent = procedure(const AIndex: Integer) of object;

function GetSpecialPathLocation: string; // get special folder location for temp files such as settings.ini and logs
function DataTypeToString(AType: TFieldType): string;

implementation

uses ShlObj, activeX, Windows, SysUtils, TypInfo;

function GetSpecialPathLocation: string;
const
  Folder = $001C; {it is CSIDL_LOCAL_APPDATA}
var
  FolderPidl: PItemIdList;

  procedure StrResetLength(var S: AnsiString);
  var
    I: Integer;
  begin
    for I := 1 to Length(S) do
      if S[I] = #0 then begin
        SetLength(S, I - 1);
        Exit;
      end;
  end;
begin
  FolderPidl := nil;
  if Succeeded(SHGetSpecialFolderLocation(0, Folder, FolderPidl)) then begin
    try
      SetLength(Result, MAX_PATH);
      if SHGetPathFromIdList(FolderPidl, PChar(Result)) then
        StrResetLength(Result)
      else
        Result := '';
    finally
      CoTaskMemFree(FolderPidl);
    end;
  end
  else
    Result := '';

  Result := Result + '\FF-RDBMS-Converter\';
  if not DirectoryExists(Result) then
    MkDir(Result);
end;

function DataTypeToString(AType: TFieldType): string;
begin
  Result := GetEnumName(TypeInfo(TFieldType), Ord(AType));
end;

function ValidateTimeStamp(const TimeStamp: TTimeStamp): Boolean;
begin
  Result := not ((TimeStamp.Time < 0) or (TimeStamp.Date <= 0));
end;

end.

