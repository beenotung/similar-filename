-module(similar).

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
  Xs = scanDir(File, []),
  lists:foreach(fun(X) ->
    {Y, Diff} = getClosest(X, Xs),
    io:format("~p~n", [#{x=>X#file.filename, y=>Y#file.filename, diff=>Diff}])
                end, Xs),
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
  io:format("Scan Dir: ~p~n", [File]),
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

getClosest(X, {Y, Diff}, []) ->
  {Y, Diff};
getClosest(X, Y, [X | Acc]) ->
  getClosest(X, Y, Acc);
getClosest(X, {Y, Y_Diff}, [Z | Acc]) ->
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
    [X, _] -> X;
    [X] -> X
  end.
