-module(model_mnesia).
-export([init/1,
	 create/2,
	 read_by_id/2,
	 read_by_query/2,
	 update/2,
	 delete/2,
	 to_query/2
	]).

%%
%% Exported
%%

init(Model) ->
    init_model(Model).

create(Model, Kvs) ->
    Default = undefined,
    Record = kvs_to_record(Model, Kvs, Default),
    ok = mnesia:dirty_write(table_name(Model), Record),
    instance(Model, Record).

read_by_id(Model, Id) ->
    case mnesia:dirty_read(table_name(Model), Id) of
	[Record] ->
	    instance(Model, Record);
	[] ->
	    throw({invalid, Model});
	[_,_|_] ->
	    error({error, bag})
    end.

read_by_query(Model, {select, Query}) ->
    Records = mnesia:dirty_select(table_name(Model), Query),
    [instance(Model, Record) || Record <- Records].

%%
%% Exported -- Used by instances
%%

update(Model, Kvs) ->
    Default = undefined,
    Record = kvs_to_record(Model, Kvs, Default),
    ok = mnesia:dirty_write(table_name(Model), Record).

delete(Model, Id) ->
    ok = mnesia:dirty_delete(table_name(Model), Id).

to_query(Model, Kvs) ->
    Head = kvs_to_dollar_record(Model, Kvs, '_'),
    Cond = to_cond(Model, Kvs),
    Body = ['$_'],
    {select, [{Head, Cond, Body}]}.

%%
%% Internal
%%

init_model(Model) ->
    Table_name = table_name(Model),
    Record_name = record_name(Model),
    Record_fields = record_fields(Model),
    ok = init_table(Table_name, Record_name, Record_fields),
    Module_name = model_module:module(Model),
    Module_name = model_module:init(Model),
    ok.

init_table(Table_name, Record_name, Record_fields) ->
    Disc_copies = mnesia:system_info(db_nodes),
    Table_options =
	[{disc_copies, Disc_copies},
	 {record_name, Record_name},
	 {attributes,  Record_fields}
	],
    Timeout = to:milliseconds({1, minute}),	% TODO: Make configurable
    case mnesia:create_table(Table_name, Table_options) of
	{atomic, ok} ->
	    ok = mnesia:wait_for_tables([Table_name], Timeout);
	{aborted, {already_exists, Table}} ->
	    ok = mnesia:wait_for_tables([Table], Timeout),
	    {value, {_, Attributes}} = lists:keysearch(attributes, 1, Table_options),
	    case Attributes =:= mnesia:table_info(Table, attributes) of
		true ->
		    ok;
		false ->
		    error({invalid, {table, Table_name}})
	    end;
	{aborted, {node_not_running, _Node}} ->
	    exit({application_not_started, mnesia});
	{aborted, {bad_type, _Table, disc_copies, _Node}} ->
	    {value, {_, Disc_copies}} = lists:keysearch(disc_copies, 1, Table_options),
	    ok = application:stop(mnesia),
	    ok = mnesia:create_schema(Disc_copies),
	    ok = application:start(mnesia),
	    ok = init_table(Table_name, Record_name, Record_fields)
    end.

to_cond(Model, Kvs) ->
    to_cond2(
      [Fvs
       ||
	  {_, Vs} = Fvs <-
	      [{Field, [V || {K, V} <- Kvs, K =:= Field]}
	       || Field <- record_fields(Model)
	      ],
	  Vs =/= []
      ],
      [],
      1).

to_cond2([{_F, [_|_] = Vs} | T], [_|_] = Acc, N) ->
    Var = dollar(N),
    Acc2 = [{'andalso', {'=:=', Var, V},  A} || A <- Acc, V <- Vs],
    to_cond2(T, Acc2, N + 1);
to_cond2([{_F, [_|_] = Vs} | T], [] = _Acc, 1 = N) ->
    Var = dollar(N),
    Acc2 = [{'=:=', Var, V} || V <- Vs],
    to_cond2(T, Acc2, N + 1);
to_cond2([{_F, [] = _Vs} | T], Acc, N) ->
    to_cond2(T, Acc, N);
to_cond2([], [_, _ | _] = Acc, _N) ->
    to_cond3(Acc, []);
to_cond2([], Acc, _N) ->
    Acc.

to_cond3([H1, H2 | T], _Acc) ->
    to_cond3(T, {'orelse', H1, H2});
to_cond3([H], Acc) ->
    {'orelse', H, Acc};
to_cond3([], Acc) ->
    Acc.

dollar(N) when N > 0 andalso N < 10 ->
    to:atom("$" ++ to:string(N)).

instance(Model, Record) ->
    Record_name = record_name(Model),
    [Record_name | Values] = tuple_to_list(Record),
    Fields = record_fields(Model),
    Kvs = lists:zip(Fields, Values),
    model_module:new(Model, Kvs).

table_name(Model) ->
    to:atom("model_" ++ to:string(Model)).

record_name(Model) ->
    table_name(Model).

record_fields(Model) ->
    {_, I_link_to} = model_conf:one2many(Model),
    Link_fields = [id_of(X) || X <- I_link_to],
    Data_fields = model_conf:data(Model),
    [id | lists:sort(Data_fields ++ Link_fields)].

id_of(Atom) when is_atom(Atom) ->
    to:atom(to:string(Atom) ++ "_id").

kvs_to_dollar_record(Model, Kvs, Default) ->
    Name = record_name(Model),
    Keys = record_fields(Model),
    kvs_to_dollar_record(Keys, Kvs, Default, [Name], 1).

kvs_to_dollar_record([Key | T], Values, Default, Acc, N) ->
    case lists:keysearch(Key, 1, Values) of
	{value, {Key, _Value}} ->
	    Var = dollar(N),
	    kvs_to_dollar_record(T, orddict:erase(Key, Values), Default, [Var | Acc], N + 1);
	false ->
	    kvs_to_dollar_record(T, Values, Default, [Default | Acc], N)
    end;
kvs_to_dollar_record([], Values, _Default, Acc, _N) ->
    [] = orddict:to_list(Values),
    list_to_tuple(lists:reverse(Acc)).

%% [{password, P}]            ==> [{salt, S}, {salted_password, Sp}]
%% [{password, P}, {salt, S}] ==> [{salt, S}, {salted_password, Sp}]
kvs_to_record(Model, Kvs, Default) ->
    Kvs2 =
	case lists:keysearch(password, 1, Kvs) of
	    {value, {_, Password} = To_be_removed} ->
		case lists:keysearch(salt, 1, Kvs) of
		    {value, {_, Salt}} ->
			{ok, Salt, Salted_password} = model_password:salt_password(Password, Salt),
			[{salted_password, Salted_password} | Kvs -- [To_be_removed]];
		    false ->
			{ok, Salt, Salted_password} = model_password:salt_password(Password),
			[{salt, Salt}, {salted_password, Salted_password} | Kvs -- [To_be_removed]]
		end;
	    false ->
		Kvs
	end,
    kvs_to_record(record_fields(Model), orddict:from_list(Kvs2), Default, [record_name(Model)]).

kvs_to_record([Key | T], Values, Default, Acc) ->
    case orddict:find(Key, Values) of
	{ok, Value} ->
	    kvs_to_record(T, orddict:erase(Key, Values), Default, [Value | Acc]);
	error ->
	    kvs_to_record(T, Values, Default, [Default | Acc])
    end;
kvs_to_record([], Values, _Default, Acc) ->
    [] = orddict:to_list(Values),
    list_to_tuple(lists:reverse(Acc)).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

kvs_to_dollar_record_test_() ->
    [?_assertEqual({rec,'_','$1','$2'}, kvs_to_dollar_record([a,b,c], [{b,b},{c,c}], '_', [rec], 1)),
     ?_assertEqual({obj,'$1','_','_','_'}, kvs_to_dollar_record([a,b,c,d], [{a,a}], '_', [obj], 1))
    ].

kvs_to_record_test_() ->
    [?_assertEqual({rec,a,undefined}, kvs_to_record([a,b],[{a,a}],undefined,[rec])),
     ?_assertEqual({ins,undefined,b,c}, kvs_to_record([a,b,c],[{b,b},{c,c}],undefined,[ins]))
    ].

setup() ->
    meck:new(model_conf),
    meck:expect(model_conf, many2many, fun(_) -> [] end),
    meck:expect(model_conf, one2many, fun(_) -> {[], []} end),
    meck:expect(model_conf, data, fun(_) -> [name] end),
    application:start(mnesia),
    [mnesia:delete_table(T) || T <- mnesia:system_info(tables), T =/= schema],
    ok.

teardown(_) ->
    [mnesia:delete_table(T) || T <- mnesia:system_info(tables), T =/= schema],
    application:stop(mnesia),
    ?assert(meck:validate(model_conf)),
    meck:unload(model_conf),
    ok.

crud_test_() ->
    Model = model,
    [{setup,
      fun setup/0,
      fun teardown/1,
      fun() ->
	      %% init
	      ?assertEqual(ok, init(Model)),
	      %% create
	      Name = "Name",
	      Name2 = "Name2",
	      Name3 = "Name3",
	      Instance = create(Model, [{id, ?LINE}, {name, Name}]),
	      ?assertEqual(Name, Instance:name()),
	      ?assertEqual(Instance, read_by_id(Model, Instance:id())),
	      %% X1 = (catch ),
	      ?assertEqual([Instance], read_by_query(Model, to_query(Model, [{id, Instance:id()}]))),
	      ?assertEqual([Instance], read_by_query(Model, to_query(Model, [{name, Instance:name()}]))),
	      ?assertEqual([], read_by_query(Model, to_query(Model, [{name, Name2}]))),
	      %% update
	      ?assertEqual([], read_by_query(Model, to_query(Model, [{name, Name2}]))),
	      ?assertEqual([], read_by_query(Model, to_query(Model, [{name, Name3}]))),
	      ?assertEqual(ok, update(Model, [{name, Name2} | Instance:to_kvs() -- [{name, Instance:name()}]])),
	      ?assertEqual([], read_by_query(Model, to_query(Model, [{name, Name}]))),
	      ?assertEqual([], read_by_query(Model, to_query(Model, [{name, Name3}]))),
	      Instance:name(Name3),
	      ?assertEqual([], read_by_query(Model, to_query(Model, [{name, Name}]))),
	      ?assertEqual([], read_by_query(Model, to_query(Model, [{name, Name2}]))),
	      ?assertEqual([], read_by_query(Model, to_query(Model, [{name, Instance:name()}]))),
	      Instance2 = read_by_id(Model, Instance:id()),
	      ?assertEqual(Name3, Instance2:name()),
	      ?assertEqual([Instance2], read_by_query(Model, to_query(Model, [{id, Instance:id()}]))),
	      ?assertEqual([Instance2], read_by_query(Model, to_query(Model, [{name, Name3}]))),
	      %% delete
	      ?assertEqual(ok, delete(Model, Instance:id())),
	      ?assertEqual([], read_by_query(Model, to_query(Model, [{id, Instance:id()}]))),
	      ?assertEqual([], read_by_query(Model, to_query(Model, [{name, Instance:name()}]))),
	      ?assertEqual([], read_by_query(Model, to_query(Model, [{id, Instance2:id()}]))),
	      ?assertEqual([], read_by_query(Model, to_query(Model, [{name, Instance2:name()}]))),
	      ?assertEqual([], read_by_query(Model, to_query(Model, [{name, Name3}]))),
	      ok
      end},
     {setup,
      fun setup/0,
      fun teardown/1,
      fun() ->
	      %% init
	      ?assertEqual(ok, init(Model)),
	      %% create
	      A = create(Model, [{id, ?LINE}, {name, "A"}]),
	      B = create(Model, [{id, ?LINE}, {name, "B"}]),
	      ?assertEqual("A", A:name()),
	      ?assertEqual("B", B:name()),
	      ?assertEqual([A], read_by_query(Model, to_query(Model, [{id,   A:id()}]))),
	      ?assertEqual([A], read_by_query(Model, to_query(Model, [{name, A:name()}]))),
	      ?assertEqual([B], read_by_query(Model, to_query(Model, [{id,   B:id()}]))),
	      ?assertEqual([B], read_by_query(Model, to_query(Model, [{name, B:name()}]))),
	      %% update
	      Name2 = "C",
	      ?assertEqual(ok, update(Model, [{id, B:id()}, {name, Name2}])),
	      ?assertEqual("B", B:name()),
	      ?assertEqual([A], read_by_query(Model, to_query(Model, [{id,   A:id()}]))),
	      ?assertEqual([A], read_by_query(Model, to_query(Model, [{name, A:name()}]))),
	      C = read_by_id(Model, B:id()),
	      ?assertEqual("C", C:name()),
	      ?assertEqual([C], read_by_query(Model, to_query(Model, [{id,   B:id()}]))),
	      ?assertEqual([ ], read_by_query(Model, to_query(Model, [{name, B:name()}]))),
	      ?assertEqual([C], read_by_query(Model, to_query(Model, [{name, Name2}]))),
	      %% delete
	      ?assertEqual(ok, delete(Model, A:id())),
	      ?assertEqual([ ], read_by_query(Model, to_query(Model, [{id,   A:id()}]))),
	      ?assertEqual([ ], read_by_query(Model, to_query(Model, [{name, A:name()}]))),
	      ?assertEqual([C], read_by_query(Model, to_query(Model, [{id,   B:id()}]))),
	      ?assertEqual([ ], read_by_query(Model, to_query(Model, [{name, B:name()}]))),
	      ?assertEqual([C], read_by_query(Model, to_query(Model, [{name, Name2}]))),
	      %% delete
	      ?assertEqual(ok, delete(Model, C:id())),
	      ?assertEqual([ ], read_by_query(Model, to_query(Model, [{id,   A:id()}]))),
	      ?assertEqual([ ], read_by_query(Model, to_query(Model, [{name, A:name()}]))),
	      ?assertEqual([ ], read_by_query(Model, to_query(Model, [{id,   B:id()}]))),
	      ?assertEqual([ ], read_by_query(Model, to_query(Model, [{name, B:name()}]))),
	      ?assertEqual([ ], read_by_query(Model, to_query(Model, [{name, Name2}]))),
	      ok
      end
     }
    ].

-endif.
