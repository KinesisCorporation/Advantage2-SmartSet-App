//CreoSource Inc. (info@creosource.com)
//Exception handling
unit u_debug;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, u_const, Dialogs;

procedure WriteExceptionLog(Filename: string; E: Exception; ExtInfo: string);
procedure HandleExcept(E: Exception; SilentMode: boolean = False;
  MsgLog: string = ''; CustomDirectory: string = '');
function CleanLogs(aPath: string): string;

implementation


//Function that cleans the log directory so it does not get filled up too quickly
function CleanLogs(aPath: string): string;
var
  TempResultDel: integer;
  TempSearchDel: TSearchRec;

  aFileList: TLogFiles;
  fileCount, fileNum: integer;
begin
  Result := '';
  aPath := IncludeTrailingBackslash(aPath);
  aFileList := nil;
  try
    try
      //Scans all files in directory
      TempResultDel := FindFirst(aPath + '*.txt', faAnyFile -
        faDirectory, TempSearchDel);
      while (TempResultDel = 0) and (Result = '') do
      begin
        SetLength(aFileList, Length(aFileList) + 1);
        aFileList[Length(aFileList) - 1].FileName := TempSearchDel.Name;
        aFileList[Length(aFileList) - 1].FileDate :=
          FileDateToDateTime(FileAge(aPath + '\' + TempSearchDel.Name));
        TempResultDel := FindNext(TempSearchDel);
      end;

      //Sorts files by date, oldest first
      SortArry(aFileList);

      //Deletes oldest log files until we reached the maximum number to keep
      fileCount := length(aFileList);
      fileNum := 0;
      while (fileCount > LOG_FILES_TO_KEEP) do
      begin
        DeleteFile(aPath + aFileList[fileNum].FileName);
        Inc(fileNum);
        Dec(fileCount);
      end;

    except
      //do nothing
    end;
  finally
    SysUtils.Findclose(TempSearchDel);
  end;
end;

//Handle exceptions an saves a log file to the subfolder "Logs"
procedure HandleExcept(E: Exception; SilentMode: boolean = False;
  MsgLog: string = ''; CustomDirectory: string = '');
var
  Filename: string;
begin
  if CustomDirectory = '' then
    CustomDirectory := GApplicationPath + 'Logs'
  else
    CustomDirectory := ExcludeTrailingBackslash(CustomDirectory);
  ForceDirectories(CustomDirectory);
  Filename := FormatDateTime('yyyymmddhhnnsszzz', Now);
  WriteExceptionLog(CustomDirectory + '\' + Filename, E, MsgLog);
  if not SilentMode then
  begin
    if MsgLog <> '' then
      MessageDlg(MsgLog, mtError, [mbOK], 0)
    else
      ApplicationShowException(E);
  end;
  CleanLogs(CustomDirectory);
end;

//Writes the info in the log file
procedure WriteExceptionLog(Filename: string; E: Exception; ExtInfo: string);
var
  LogFile: TextFile;
  DebugInfo: TStringList;
  i: integer;
const
  CDELIMITER = '---------------------------------------------------------------------';
begin
  DebugInfo := TStringList.Create;
  try
    if ExtInfo <> '' then
    begin
      DebugInfo.Add(ExtInfo);
      DebugInfo.Add(CDELIMITER);
    end;
    DebugInfo.Add('Current date/time: ' + FormatDateTime('YYYY/MM/DD, HH:NN:SS', Now));
    DebugInfo.Add('Exception class  : ' + E.ClassName);
    DebugInfo.Add('Exception message: ' + E.Message);

    DebugInfo.Add(CDELIMITER);
    {$ifdef Win32}
    DebugInfo.Add('Windows version: ' + IntToStr(Win32MajorVersion) +
      '.' + IntToStr(Win32MinorVersion));
    {$else}
    {$ifdef Darwin}
     DebugInfo.Add('MacOS');
    {$endif}
    {$endif}

    try
      if not directoryExists(ExtractFilePath(Filename + '.txt')) then
        CreateDir(ExtractFilePath(Filename + '.txt'));
      AssignFile(LogFile, Filename + '.txt');
      Rewrite(LogFile);

      for i := 0 to DebugInfo.Count - 1 do
        Writeln(LogFile, DebugInfo[i]);

      Flush(LogFile);
      CloseFile(LogFile);
    except
    end;
  finally
    DebugInfo.Free;
  end;
end;

end.
