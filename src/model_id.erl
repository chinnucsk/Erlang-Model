-module(model_id).
-export([generate/0,
	 generate_string/0,
	 to_integer/1,
	 to_string/1
	]).

-type id() :: integer() | string().

-spec generate() -> integer().
generate() ->
    to_integer(generate_string(), 0).

-spec generate_string() -> string().
generate_string() ->
    {Mega_sec, Sec, Micro_sec} = erlang:now(),
    Uuid = uuid:to_string(uuid:srandom()),
    lists:flatten(
      io_lib:format(
	"~5.16.0b-~5.16.0b-~5.16.0b-~s",
	[Mega_sec, Sec, Micro_sec, Uuid])).

-spec to_integer(id()) -> integer().
to_integer(Id) when is_integer(Id) ->
    Id;
to_integer(String) when is_list(String) ->
    to_integer(String, 0).

to_integer([H | T], Acc) when H >= $0 andalso H =< $9 ->
    to_integer(T, (Acc bsl 4) + H - $0);
to_integer([H | T], Acc) when H >= $a andalso H =< $f ->
    to_integer(T, (Acc bsl 4) + H - $a + 10);
to_integer([H | T], Acc) when H >= $A andalso H =< $F ->
    to_integer(T, (Acc bsl 4) + H - $A + 10);
to_integer([$- | T], Acc) ->
    to_integer(T, Acc);
to_integer([], Acc) ->
    Acc.

-spec to_string(id()) -> string().
to_string(String) when is_list(String) ->
    String;
to_string(Id) when is_integer(Id) ->
    to_string(Id, [], 1).

to_string(Id, Acc, N)
  when
      N =:= 13 orelse
      N =:= 18 orelse
      N =:= 23 orelse
      N =:= 28 orelse
      N =:= 37 orelse
      N =:= 43 orelse
      N =:= 49
      ->
    to_string(Id, [$- | Acc], N + 1);
to_string(Id, Acc, N) when N > 0 andalso N < 55 ->
    A = case Id band 15 of
	    D when D < 10 -> D + $0;
	    D when D >= 10 -> D + $a - 10
	end,
    to_string(Id bsr 4, [A | Acc], N + 1);
to_string(0, Acc, 55) ->
    Acc.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

string_test_() ->
    Re_options = [global, {capture,all_but_first,list}],
    [?_assertMatch([5,5,5,8,4,4,4,12], [length(T) || T <- string:tokens(generate_string(),"-")]),
     ?_assertMatch([5,5,5,8,4,4,4,12], [length(T) || T <- string:tokens(to_string(generate()),"-")]),
     ?_assertMatch({match,[_,_,_,_,_,_,_,_]}, re:run(generate_string(), "([[:xdigit:]]+)", Re_options)),
     ?_assertMatch({match,[_,_,_,_,_,_,_,_]}, re:run(to_string(generate()), "([[:xdigit:]]+)", Re_options)),
     fun() ->
	     String = generate_string(),
	     ?assertEqual(String, to_string(to_integer(String)))
     end,
     fun() ->
	     Integer = generate(),
	     ?assertEqual(Integer, to_integer(to_string(Integer)))
     end
    ].

-endif.
