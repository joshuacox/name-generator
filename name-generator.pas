program NameGenerator;
{ This is the sole Pascal source file for the project }

{$mode objfpc}{$H+}
uses
  SysUtils, Classes, Process, StrUtils;

{-----------------------------------------------
  Helper routines
------------------------------------------------}

// Return true if a path points to a regular file
function IsRegularFile(const Path: string): Boolean;
var
  SR: TSearchRec;
begin
  Result := FindFirst(Path, faAnyFile, SR) = 0;
  if Result then
    Result := (SR.Attr and faDirectory) = 0;
  FindClose(SR);
end;

// Return a list of regular files (full paths) inside a folder
function ListRegularFiles(const Folder: string): TStringList;
var
  SR: TSearchRec;
  FullPath: string;
begin
  Result := TStringList.Create;
  if FindFirst(IncludeTrailingPathDelimiter(Folder) + '*', faAnyFile, SR) = 0 then
  begin
    repeat
      if (SR.Attr and faDirectory) = 0 then
      begin
        FullPath := IncludeTrailingPathDelimiter(Folder) + SR.Name;
        Result.Add(FullPath);
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
end;

// Pick a random element from a string list (list must not be empty)
function RandomChoice(const List: TStringList): string;
begin
  Result := List[Random(List.Count)];
end;

// Pick a random regular file from a folder (raises if none)
function PickRandomFile(const Folder: string): string;
var
  Files: TStringList;
begin
  Files := ListRegularFiles(Folder);
  try
    if Files.Count = 0 then
      raise Exception.CreateFmt('Folder "%s" contains no regular files', [Folder]);
    Result := RandomChoice(Files);
  finally
    Files.Free;
  end;
end;

// Resolve an environment variable to a file path, falling back to a random file
function ResolveFile(const EnvVar, Folder: string): string;
var
  EnvPath: string;
begin
  EnvPath := GetEnv(EnvVar);
  if (EnvPath <> '') then
  begin
    EnvPath := ExpandFileName(EnvPath);
    if not IsRegularFile(EnvPath) then
      raise Exception.CreateFmt('Environment variable %s points to a non‑regular file: %s',
                                [EnvVar, EnvPath]);
    Result := EnvPath;
  end
  else
    Result := PickRandomFile(Folder);
end;

// Read a file, return a list of non‑empty trimmed lines
function ReadNonEmptyLines(const FileName: string): TStringList;
var
  SL: TStringList;
  i: Integer;
  Line: string;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(FileName);
    Result := TStringList.Create;
    for i := 0 to SL.Count - 1 do
    begin
      Line := Trim(SL[i]);
      if Line <> '' then
        Result.Add(Line);
    end;
  finally
    SL.Free;
  end;
end;

// Try to obtain terminal height via `tput lines`
function TputLines: Integer;
var
  Proc: TProcess;
  Output: string;
begin
  Result := -1;  // sentinel for “not available”
  Proc := TProcess.Create(nil);
  try
    Proc.Executable := 'tput';
    Proc.Parameters.Add('lines');
    Proc.Options := [poUsePipes, poNoConsole];
    Proc.Execute;
    SetLength(Output, Proc.Output.NumBytesAvailable);
    Proc.Output.Read(Output[1], Length(Output));
    Proc.WaitOnExit;
    Output := Trim(Output);
    if Output <> '' then
      if TryStrToInt(Output, Result) then
        Exit;
  finally
    Proc.Free;
  end;
end;

// Determine how many names to generate (COUNT_O)
function GetCountO: Integer;
var
  EnvVal: string;
  Tmp: Integer;
begin
  // 1️⃣ env var `counto` (preferred)
  EnvVal := GetEnv('counto');
  if (EnvVal <> '') and TryStrToInt(EnvVal, Tmp) then
    Exit(Tmp);

  // 2️⃣ `tput lines`
  Tmp := TputLines;
  if Tmp > 0 then
    Exit(Tmp);

  // 3️⃣ fallback
  Result := 24;
end;

// Print debug information to STDERR (mirrors the shell script)
procedure MaybeDebug(const Adj, Noun, AdjFile, NounFile,
                    AdjFolder, NounFolder: string; CountZero, CountO: Integer);
begin
  if GetEnv('DEBUG') = 'true' then
  begin
    WriteLn(StdErr, Adj);
    WriteLn(StdErr, Noun);
    WriteLn(StdErr, AdjFile);
    WriteLn(StdErr, AdjFolder);
    WriteLn(StdErr, NounFile);
    WriteLn(StdErr, NounFolder);
    WriteLn(StdErr, Format('%d > %d', [CountZero, CountO]));
  end;
end;

// Helper: Get environment variable with default fallback
function GetEnvOrDefault(const Name, Default: string): string;
var
  Val: string;
begin
  Val := GetEnv(Name);
  if Val = '' then
    Result := Default
  else
    Result := Val;
end;

{-----------------------------------------------
  Main program
------------------------------------------------}
var
  HERE, NOUN_FOLDER, ADJ_FOLDER: string;
  NOUN_FILE, ADJ_FILE: string;
  Nouns, Adjectives: TStringList;
  CountO, CountZero: Integer;
  Sep: string;
  RandomNoun, RandomAdj: string;
begin
  Randomize;

  // -----------------------------------------------
  // Configuration – environment overrides with sensible defaults
  // -----------------------------------------------
  HERE := GetCurrentDir;

  NOUN_FOLDER := GetEnvOrDefault('NOUN_FOLDER',
    IncludeTrailingPathDelimiter(HERE) + 'nouns');

  ADJ_FOLDER := GetEnvOrDefault('ADJ_FOLDER',
    IncludeTrailingPathDelimiter(HERE) + 'adjectives');

  // Resolve files – env var overrides, otherwise pick a random file from the folder.
  NOUN_FILE := ResolveFile('NOUN_FILE', NOUN_FOLDER);
  ADJ_FILE  := ResolveFile('ADJ_FILE',  ADJ_FOLDER);

  // Separator (default "-")
  Sep := GetEnvOrDefault('SEPARATOR', '-');

  // Number of lines to emit
  CountO := GetCountO;

  // Load the word lists (skip empty lines)
  Nouns := ReadNonEmptyLines(NOUN_FILE);
  Adjectives := ReadNonEmptyLines(ADJ_FILE);
  try
    CountZero := 0;
    while CountZero < CountO do
    begin
      // Pick a random noun and lower‑case it
      RandomNoun := RandomChoice(Nouns);
      RandomNoun := LowerCase(RandomNoun);

      // Pick a random adjective (preserve case)
      RandomAdj := RandomChoice(Adjectives);

      // Debug output (mirrors the shell script's debugger)
      MaybeDebug(RandomAdj, RandomNoun, ADJ_FILE, NOUN_FILE,
                 ADJ_FOLDER, NOUN_FOLDER, CountZero, CountO);

      // Emit the generated name
      WriteLn(RandomAdj + Sep + RandomNoun);

      Inc(CountZero);
    end;
  finally
    Nouns.Free;
    Adjectives.Free;
  end;
end.
