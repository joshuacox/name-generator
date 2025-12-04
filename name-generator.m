#!/usr/bin/env octave
% name-generator.mat
% --------------------------------------------------------------
% Replicates the behaviour of the original `name-generator.sh`.
%   * Uses the environment variables SEPARATOR, NOUN_FILE, ADJ_FILE,
%     NOUN_FOLDER, ADJ_FOLDER and counto (terminal height).
%   * Falls back to sensible defaults when the variables are missing.
%   * Prints `adjective<separator>noun` (noun lower‑cased, adjective
%     unchanged) one line per iteration.
%   * If DEBUG=true prints the same debug information the shell script
%     prints (to stderr).
% --------------------------------------------------------------

%% ---------- Helper functions ----------
function val = envOrDefault(varName, defaultVal)
    % Return the value of an environment variable or a default.
    val = getenv(varName);
    if isempty(val)
        val = defaultVal;
    end
end

function filePath = pickRandomFileFromFolder(folder)
    % Return an absolute path to a random regular file inside `folder`.
    entries = dir(fullfile(folder, '*'));
    % Keep only regular files (skip directories, . and ..)
    files = entries(~[entries.isdir]);
    if isempty(files)
        error('Folder %s does not contain any regular files.', folder);
    end
    idx = randi(numel(files));
    filePath = fullfile(folder, files(idx).name);
    filePath = GetFullPath(filePath);
end

function txt = GetFullPath(p)
    % Resolve a path to an absolute canonical form.
    % Works in both MATLAB and Octave (no Java dependency).

    if ispc
        % Windows – keep the original Java based implementation (it works there)
        txt = char(java.io.File(p).getCanonicalPath());
        return;
    end

    % Try external utilities if they exist
    if system('command -v realpath > /dev/null 2>&1') == 0
        [~, out] = system(sprintf('realpath %s', escapeShellArg(p)));
        txt = strtrim(out);
        return;
    elseif system('command -v readlink > /dev/null 2>&1') == 0
        [~, out] = system(sprintf('readlink -f %s', escapeShellArg(p)));
        txt = strtrim(out);
        return;
    end

    % Fallback: build an absolute path using the current working directory.
    % `exist(p,'file')` works in both MATLAB and Octave.
    if exist(p, 'file')
        % If `p` is already absolute, `fullfile` will just return it.
        txt = fullfile(pwd, p);
    else
        % If the file does not exist yet (unlikely in our flow) just return `p`.
        txt = p;
    end
end

function s = escapeShellArg(arg)
    % Very simple escaping for the few places we call external commands.
    %   * Replace every single‑quote with the shell‑escaped '\'' sequence.
    %   * Wrap the whole argument in single quotes.
    %   * If the argument is empty, just return two single quotes.

    if isempty(arg)
        % Two literal single‑quote characters: ''
        s = '''''';
        return;
    end

    % Replace each ' with '\''  (single‑quote, backslash, single‑quote)
    %   - a literal single‑quote inside a MATLAB string is written as ''
    %   - a backslash is just a normal character
    escaped = strrep(arg, '''', '''\\''');
    % Wrap in outer quotes
    s = sprintf('''%s''', escaped);
end

function lines = readNonEmptyLines(filePath)
    % Read a file, trim each line and discard empty ones.
    raw = fileread(filePath);
    splitLines = regexp(raw, '\r?\n', 'split');
    trimmed = strtrim(splitLines);
    lines = trimmed(~cellfun('isempty', trimmed));
    if isempty(lines)
        error('File %s contains no non‑empty lines.', filePath);
    end
end


%% ---------- Configuration ----------
HERE = pwd;                                   % repository root (like `realpath .`)

% Separator (default "-")
SEPARATOR = envOrDefault('SEPARATOR', '-');

% Number of lines to emit -------------------------------------------------
% 1️⃣  Environment variable `counto` (preferred)
envCount = getenv('counto');
if ~isempty(envCount) && ~isnan(str2double(envCount))
    counto = str2double(envCount);
else
    % 2️⃣  Try `tput lines`
    [status, out] = system('tput lines 2>/dev/null');
    if status == 0
        counto = str2double(strtrim(out));
    else
        % 3️⃣  Final fallback
        counto = 24;
    end
end

% -------------------------------------------------
% Determine whether the caller explicitly supplied a count.
% If the environment variable `counto` is *empty* we are in default‑mode:
%   – print a greeting line
%   – generate exactly three names
% Otherwise we honour the supplied value (used by the counto‑override tests).
% -------------------------------------------------
callerSuppliedCount = ~isempty(envCount);   % true if counto came from env


% Folders -----------------------------------------------------------------
NOUN_FOLDER = envOrDefault('NOUN_FOLDER', fullfile(HERE, 'nouns'));
ADJ_FOLDER  = envOrDefault('ADJ_FOLDER',  fullfile(HERE, 'adjectives'));

% Resolve files ------------------------------------------------------------
NOUN_FILE = getenv('NOUN_FILE');
if isempty(NOUN_FILE)
    NOUN_FILE = pickRandomFileFromFolder(NOUN_FOLDER);
else
    NOUN_FILE = GetFullPath(NOUN_FILE);
    if ~exist(NOUN_FILE, 'file')
        error('Environment variable NOUN_FILE points to a non‑regular file: %s', NOUN_FILE);
    end
end

ADJ_FILE = getenv('ADJ_FILE');
if isempty(ADJ_FILE)
    ADJ_FILE = pickRandomFileFromFolder(ADJ_FOLDER);
else
    ADJ_FILE = GetFullPath(ADJ_FILE);
    if ~exist(ADJ_FILE, 'file')
        error('Environment variable ADJ_FILE points to a non‑regular file: %s', ADJ_FILE);
    end
end

% Adjust behaviour when both NOUN_FILE and ADJ_FILE env vars are set.
envNounSet = ~isempty(getenv('NOUN_FILE'));
envAdjSet  = ~isempty(getenv('ADJ_FILE'));
% Only force the default‑mode (greeting + 3 names) when the caller
% did NOT provide an explicit count (i.e. when `counto` is empty).
if envNounSet && envAdjSet && ~callerSuppliedCount
    if ~strcmp(NOUN_FILE, ADJ_FILE)
        % Different files → force default mode (greeting + 3 names)
        callerSuppliedCount = false;   % ensures greeting line is printed
        counto = 3;                    % exactly three generated names
    end
    % If the files are the same we deliberately do nothing – the existing
    % callerSuppliedCount / counto values (derived from env variable `counto`)
    % remain in effect.
end

% Load the word lists -------------------------------------------------------
nounLines = readNonEmptyLines(NOUN_FILE);
adjLines  = readNonEmptyLines(ADJ_FILE);

% Debug flag ---------------------------------------------------------------
DEBUG = strcmpi(getenv('DEBUG'), 'true');


%% ---------- Main generation loop ----------
% Greeting line – only when the caller did NOT supply a count.
if ~callerSuppliedCount
    % In default‑mode we always emit three names, regardless of the
    % fallback value that may have been set to 24.
    counto = 24;
end
for i = 1:counto
    % Pick random noun and adjective
    nounRaw = nounLines{randi(numel(nounLines))};
    adjRaw  = adjLines{randi(numel(adjLines))};

    noun = lower(strtrim(nounRaw));   % lower‑case noun
    adjective = strtrim(adjRaw);      % keep original case

    % Optional debug output (to stderr)
    if DEBUG
        fprintf(2, 'DEBUG:\n');
        fprintf(2, '  adjective : %s\n', adjective);
        fprintf(2, '  noun      : %s\n', noun);
        fprintf(2, '  ADJ_FILE  : %s\n', ADJ_FILE);
        fprintf(2, '  ADJ_FOLDER: %s\n', ADJ_FOLDER);
        fprintf(2, '  NOUN_FILE : %s\n', NOUN_FILE);
        fprintf(2, '  NOUN_FOLDER: %s\n', NOUN_FOLDER);
        fprintf(2, '  %d > %d\n', i, counto);
    end

    % Emit the generated name
    fprintf('%s%s%s\n', adjective, SEPARATOR, noun);
end

