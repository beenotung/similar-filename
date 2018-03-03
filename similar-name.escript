#!/usr/bin/env escript
-export([main/0, main/1]).

-record(file, {
  path
  , name
  , filename
  , digest
  , length
}).

help() ->
  io:format(standard_error, "Expect zero or one argument.~n", []).

main() ->
  runDir(".").

main(Args) ->
  case Args of
    [Dir0] ->
      Dir = erlang:atom_to_list(Dir0),
      case Dir of
        [$/ | _] ->
          runDir(Dir);
        _ ->
          runDir("./" ++ Dir)
      end;
    [] ->
      runDir(".");
    _ ->
      help()
  end.

runDir(File) ->
  List1 = scanDir(File, []),
  io:format(standard_error, "finding closest filename", []),
  List2 = lists:map(fun(X) ->
    io:format(standard_error, ".", []),
    {Y, Diff} = getClosest(X, List1),
    {X, Y, Diff}
                    end, List1),
  io:format(standard_error, "~n", []),
  io:format(standard_error, "de-duplicating...~n", []),
  Dict1 = lists:foldl(fun({X, Y, _Diff} = C, Acc) ->
    Key = if
            X#file.path < Y#file.path ->
              {X#file.path, Y#file.path};
            true ->
              {Y#file.path, X#file.path}
          end,
    Acc#{Key=>C}
                      end, #{}, List2),
  List3 = maps:values(Dict1),
  io:format(standard_error, "sorting...~n", []),
  List4 = lists:sort(fun({_, _, A}, {_, _, B}) -> A < B end, List3),
  lists:foreach(fun({X, Y, Diff}) ->
    io:format("~.3f~n  ~s | ~s~n  ~s | ~s~n", [
      Diff
      , unicode:characters_to_binary(X#file.filename)
      , unicode:characters_to_binary(X#file.path)
      , unicode:characters_to_binary(Y#file.filename)
      , unicode:characters_to_binary(Y#file.path)
    ])
                end, List4),
  ok.

scan(File, Name, Acc) ->
  case filelib:is_dir(File) of
    true -> scanDir(File, Acc);
    false -> scanFile(File, Name, Acc)
  end.

scanFile(File, FileName, Acc) ->
  Name = getName(FileName),
%%  io:format("~s~n", [unicode:characters_to_binary(Name)]),
  [#file{
    path = File
    , name = Name
    , filename = FileName
    , length = length(Name)
    , digest = digestWord(Name)
  } | Acc].


scanDir(File, Acc0) ->
  io:format(standard_error, "Scan Dir: ~s~n", [unicode:characters_to_binary(File)]),
  {ok, Fs} = file:list_dir(File),
  lists:foldl(fun(F, Acc) -> scan(File ++ "/" ++ F, F, Acc) end, Acc0, Fs).

digestWord(S) ->
  lists:foldl(fun(H, Acc) ->
    C = maps:get(H, Acc, 0),
    Acc#{H=>C + 1}
              end, #{}, S).

compareDigest(A, B) ->
  sets:fold(
    fun(C, Acc) ->
      Acc + abs(maps:get(C, A, 0) - maps:get(C, B, 0))
    end
    , 0
    , sets:union(
      sets:from_list(maps:keys(A))
      , sets:from_list(maps:keys(B))
    )).

getClosest(X, Acc) ->
  getClosest(X, {X, 0}, Acc).

getClosest(_X, {Y, Diff}, []) ->
  {Y, Diff};
getClosest(X, Y, [X | Acc]) ->
  getClosest(X, Y, Acc);
getClosest(X, {Y, Y_Diff}, [Z | Acc]) ->
%%  io:format("~p~n", [#{x=>X, y=>Y}]),
  Z_Diff = compareDigest(X#file.digest, Z#file.digest) / (X#file.length + Z#file.length),
  C = if
        X == Y ->
          {Z, Z_Diff};
        Y_Diff > Z_Diff ->
          {Z, Z_Diff};
        Y_Diff =< Z_Diff ->
          {Y, Y_Diff}
      end,
  getClosest(X, C, Acc).

getName(S) ->
  case string:split(S, ".", trailing) of
    [[], _] -> S;
    [X, _] -> X;
    [X] -> X
  end.
