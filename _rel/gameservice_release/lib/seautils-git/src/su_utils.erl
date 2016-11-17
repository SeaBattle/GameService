%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(su_utils).
-author("tihon").

%% API
-export([
  to_lower/1,
  get_priv_dir/1,
  get_random_non_numeric_string/1,
  hexstring/1,
  hash_sha256/1,
  hash_secret/1,
  bin_to_hexstr/1,
  get_random_element/1]).

-spec to_lower(binary() | string()) -> binary() | string().
to_lower(Bin) when is_binary(Bin) ->
  list_to_binary(to_lower(binary_to_list(Bin)));
to_lower(List) -> string:to_lower(List).

-spec get_priv_dir(atom()) -> string().
get_priv_dir(App) ->
  Path = case application:get_key(App, vsn) of
           {ok, VSNString} ->
             "./lib/" ++ atom_to_list(App) ++ "-" ++ VSNString;
           undefined ->
             {ok, Dir} = file:get_cwd(),
             Dir
         end,
  Path ++ "/priv/".

get_random_element([]) -> undefined;
get_random_element([Element]) -> Element;
get_random_element(Elements) ->
  random:seed(),
  N = random:uniform(length(Elements)),
  lists:nth(N, Elements).

-spec get_random_non_numeric_string(non_neg_integer()) -> string().
get_random_non_numeric_string(MaxLen) ->
  String = string:to_lower(uuid:to_string(uuid:uuid4())),
  Filtered = lists:filter(
    fun(A) ->
      not (A =:= $0
        orelse A =:= $1
        orelse A =:= $2
        orelse A =:= $3
        orelse A =:= $4
        orelse A =:= $5
        orelse A =:= $6
        orelse A =:= $7
        orelse A =:= $8
        orelse A =:= $9
        orelse A =:= $o
        orelse A =:= $l
        orelse A =:= $-)
    end, String),
  if
    length(Filtered) < MaxLen -> get_random_non_numeric_string(MaxLen);
    true ->
      {Secret, _} = lists:split(MaxLen, Filtered),
      Secret
  end.

-spec hash_secret(string()) -> binary().
hash_secret(Secret) ->
  list_to_binary(string:to_upper(hash_sha256(Secret))).

-spec hash_sha256(binary() | string()) -> binary().
hash_sha256(Data) ->
  list_to_binary(hexstring(crypto:hash(sha256, Data))).

-spec bin_to_hexstr(binary()) -> string().
bin_to_hexstr(Bin) ->
  lists:flatten([io_lib:format("~2.16.0B", [X]) ||
    X <- binary_to_list(Bin)]).

hexstring(<<X:128/big-unsigned-integer>>) ->
  lists:flatten(io_lib:format("~32.16.0b", [X]));
hexstring(<<X:160/big-unsigned-integer>>) ->
  lists:flatten(io_lib:format("~40.16.0b", [X]));
hexstring(<<X:256/big-unsigned-integer>>) ->
  lists:flatten(io_lib:format("~64.16.0b", [X]));
hexstring(<<X:512/big-unsigned-integer>>) ->
  lists:flatten(io_lib:format("~128.16.0b", [X])).