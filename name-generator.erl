%%--------------------------------------------------------------------
%% @doc
%%   Erlang implementation of the name-generator shell script.
%%   It respects the following environment variables:
%%     SEPARATOR   – string placed between adjective and noun (default "-")
%%     counto      – number of names to emit (default: terminal height)
%%     NOUN_FOLDER – folder containing noun files (default: $PWD/nouns)
%%     ADJ_FOLDER  – folder containing adjective files (default: $PWD/adjectives)
%%     NOUN_FILE   – specific noun file to use (default: random file from NOUN_FOLDER)
%%     ADJ_FILE    – specific adjective file to use (default: random file from ADJ_FOLDER)
%%     DEBUG       – if set to "true", prints debugging information.
%%--------------------------------------------------------------------
-module(name_generator).
-export([main/0]).

-include_lib("kernel/include/file.hrl").

%%====================================================================
%% Entry point
%%====================================================================
main() ->
    %% Seed the random generator
    _ = rand:seed(exsplus, now_timestamp()),

    Separator   = get_env("SEPARATOR", "-"),
    CountO      = get_counto(),
    NounFolder  = get_env_path("NOUN_FOLDER", default_noun_folder()),
    AdjFolder   = get_env_path("ADJ_FOLDER",  default_adj_folder()),
    NounFile    = get_env_path("NOUN_FILE", pick_random_file(NounFolder)),
    AdjFile     = get_env_path("ADJ_FILE",  pick_random_file(AdjFolder)),
    Debug       = get_env("DEBUG", "false") =:= "true",

    NounLines = read_nonempty_lines(NounFile),
    AdjLines  = read_nonempty_lines(AdjFile),

    generate_names(CountO, Separator, NounLines, AdjLines, Debug,
                   NounFile, AdjFile, NounFolder, AdjFolder).

%%====================================================================
%% Helpers
%%====================================================================

%% Get an environment variable or return Default.
get_env(Var, Default) ->
    case os:getenv(Var) of
        false -> Default;
        Value -> Value
    end.

%% Get an environment variable interpreted as a path, or return DefaultPath.
get_env_path(Var, DefaultPath) ->
    case os:getenv(Var) of
        false -> DefaultPath;
        ""    -> DefaultPath;
        Value -> filename:absname(Value)
    end.

%% Resolve the current working directory.
default_cwd() ->
    {ok, Cwd} = file:get_cwd(),
    Cwd.

default_noun_folder() ->
    filename:join(default_cwd(), "nouns").

default_adj_folder() ->
    filename:join(default_cwd(), "adjectives").

%% Determine how many names to emit.
get_counto() ->
    case os:getenv("counto") of
        false -> terminal_lines();
        ""    -> terminal_lines();
        Str   ->
            case string:to_integer(string:trim(Str)) of
                {error, _} -> terminal_lines();
                {Int, _}   -> Int
            end
    end.

%% Try to obtain terminal height via `tput lines`; fallback to 24.
terminal_lines() ->
    case os:cmd("tput lines") of
        "" -> 24;
        Out ->
            case string:to_integer(string:trim(Out)) of
                {error, _} -> 24;
                {Int, _}   -> Int
            end
    end.

%% Pick a random regular file from a folder.
pick_random_file(Folder) ->
    case list_regular_files(Folder) of
        [] -> erlang:error({no_regular_files, Folder});
        Files -> lists:nth(rand_index(length(Files)), Files)
    end.

list_regular_files(Folder) ->
    case file:list_dir(Folder) of
        {ok, Entries} ->
            [ filename:join(Folder, E)
              || E <- Entries,
                 filelib:is_regular(filename:join(Folder, E)) ];
        {error, _} -> []
    end.

rand_index(N) when N > 0 ->
    rand:uniform(N).

%% Read a file and return a list of non‑empty trimmed lines.
read_nonempty_lines(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Bin} ->
            Content = unicode:characters_to_list(Bin),
            Lines = string:split(Content, "\n", all),
            [ string:trim(L) || L <- Lines, string:trim(L) =/= "" ];
        {error, Reason} ->
            erlang:error({cannot_read_file, FilePath, Reason})
    end.

%% Generate and print the required number of names.
generate_names(0, _Sep, _NounLines, _AdjLines, _Debug,
               _NounFile, _AdjFile, _NounFolder, _AdjFolder) ->
    ok;
generate_names(Count, Sep, NounLines, AdjLines, Debug,
               NounFile, AdjFile, NounFolder, AdjFolder) ->
    NounRaw = random_line(NounLines),
    AdjRaw  = random_line(AdjLines),

    Noun = string:to_lower(NounRaw),
    Adj  = AdjRaw,

    maybe_debug(Debug, Adj, Noun, AdjFile, AdjFolder,
                NounFile, NounFolder, Count, Count),

    io:format("~s~s~s~n", [Adj, Sep, Noun]),
    generate_names(Count - 1, Sep, NounLines, AdjLines, Debug,
                   NounFile, AdjFile, NounFolder, AdjFolder).

random_line(Lines) ->
    Index = rand_index(length(Lines)),
    lists:nth(Index, Lines).

maybe_debug(true, Adj, Noun, AdjFile, AdjFolder,
           NounFile, NounFolder, CountZero, CountO) ->
    io:format("DEBUG:~n", []),
    io:format("  adjective : ~s~n", [Adj]),
    io:format("  noun      : ~s~n", [Noun]),
    io:format("  ADJ_FILE  : ~s~n", [AdjFile]),
    io:format("  ADJ_FOLDER: ~s~n", [AdjFolder]),
    io:format("  NOUN_FILE : ~s~n", [NounFile]),
    io:format("  NOUN_FOLDER: ~s~n", [NounFolder]),
    io:format("  ~p > ~p~n", [CountZero, CountO]);
maybe_debug(false, _Adj, _Noun, _AdjFile, _AdjFolder,
           _NounFile, _NounFolder, _CountZero, _CountO) ->
    ok.

%% Helper to get a timestamp suitable for rand:seed/2.
now_timestamp() ->
    {Mega, Sec, Micro} = erlang:now(),
    {Mega, Sec, Micro}.
