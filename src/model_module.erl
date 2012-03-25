-module(model_module).
-export([init/1,
	 module/1,
	 new/2
	]).

%%
%% Exported
%%

-type model() :: atom().
-spec init(model()) -> module().
init(Model) ->
    Module = module(Model),
    case code:is_loaded(Module) of
	{file, _} ->
	    Module;

	false ->
	    {ok, M1} = smerl:for_module(model_mnesia_smerl_template),

	    %%
	    %% Extract functions
	    %%

	    {ok, New}				= smerl:get_func(M1, new, 1),
	    {ok, Update}			= smerl:get_func(M1, update, 0),
	    {ok, Delete}			= smerl:get_func(M1, delete, 0),

	    {ok, Get_data}			= smerl:get_func(M1, get_data, 0),
	    {ok, Set_data}			= smerl:get_func(M1, set_data, 1),
	    {ok, Set_password}			= smerl:get_func(M1, set_password, 1),

	    {ok, Add_one2many}			= smerl:get_func(M1, add_one2many, 1),
	    {ok, Get_one2many_ids}		= smerl:get_func(M1, get_one2many_ids, 0),
	    {ok, Get_one2many_instances}	= smerl:get_func(M1, get_one2many_instances, 0),
	    {ok, Remove_one2many}		= smerl:get_func(M1, remove_one2many, 2),
	    {ok, Get_many2one_id}		= smerl:get_func(M1, get_many2one_id, 0),
	    {ok, Get_many2one_instance}		= smerl:get_func(M1, get_many2one_instance, 0),
	    {ok, Set_many2one}			= smerl:get_func(M1, set_many2one, 1),

	    {ok, Add_many2many}			= smerl:get_func(M1, add_many2many, 1),
	    {ok, Get_many2many_ids}		= smerl:get_func(M1, get_many2many_ids, 0),
	    {ok, Get_many2many_instances}	= smerl:get_func(M1, get_many2many_instances, 0),
	    {ok, Remove_many2many}		= smerl:get_func(M1, remove_many2many, 1),

	    %%
	    %% Clear module
	    %%

	    M2	= smerl:remove_func(M1,  new, 1),
	    M3	= smerl:remove_func(M2,  update, 0),
	    M4	= smerl:remove_func(M3,  delete, 0),
	    M5	= smerl:remove_func(M4,  get_data, 0),
	    M6	= smerl:remove_func(M5,  set_data, 1),
	    M7	= smerl:remove_func(M6,  set_password, 1),
	    M8	= smerl:remove_func(M7,  add_one2many, 1),
	    M9	= smerl:remove_func(M8,  get_one2many_ids, 0),
	    M10 = smerl:remove_func(M9,  get_one2many_instances, 0),
	    M11 = smerl:remove_func(M10, remove_one2many, 2),
	    M12 = smerl:remove_func(M11, get_many2one_id, 0),
	    M13 = smerl:remove_func(M12, get_many2one_instance, 0),
	    M14 = smerl:remove_func(M13, set_many2one, 1),
	    M15 = smerl:remove_func(M14, add_many2many, 1),
	    M16 = smerl:remove_func(M15, get_many2many_ids, 0),
	    M17 = smerl:remove_func(M16, get_many2many_instances, 0),
	    M18 = smerl:remove_func(M17, remove_many2many, 1),

	    %%
	    %% Rename module
	    %%

	    M19 = smerl:set_module(M18, {Module, ['Context']}),

	    %%
	    %% Add updated general functions
	    %%

	    {ok, M20} = smerl:add_func(M19, New, true),

	    Update2 = replace([{model, Model}], Update),
	    {ok, M21} = smerl:add_func(M20, Update2, true),

	    Many2many_fields = abstract_list(model_conf:many2many(Model), 1),
	    One2may_fields = abstract_list(element(1, model_conf:one2many(Model)), 1),

	    Delete2 =
		replace(
		  [{model, Model},
		   {many2many_fields, Many2many_fields},
		   {one2may_fields, One2may_fields},
		   {model_id, id_of(Model)}
		  ],
		  Delete),
	    {ok, M22} = smerl:add_func(M21, Delete2, true),

	    %%
	    %% Add updated data functions
	    %%

	    {ok, M23} = smerl:add_func(M22, replace([{get_data, id}, {field, id}], Get_data)),

	    {ok, M24} =
		lists:foldl(
		  fun(Field, {ok, Ma}) ->
			  Get_data2 = replace([{get_data, Field}, {field, Field}], Get_data),
			  Set_data2 = replace([{set_data, Field}, {field, Field}], Set_data),
			  {ok,  Mb} = smerl:add_func(Ma, Get_data2, true),
			  {ok, _Mc} = smerl:add_func(Mb, Set_data2, true)
		  end,
		  {ok, M23},
		  model_conf:data(Model) -- [salt, salted_password]),

	    {ok, M25} =
		case [salt, salted_password] -- model_conf:data(Model) of
		    [] ->
			%% We have a password !
			fun({ok, Ma}) ->
				Get_salt = replace([{get_data, salt}, {field, salt}], Get_data),
				Get_salted_password =
				    replace(
				      [{get_data, salted_password}, {field, salted_password}],
				      Get_data),
				Set_password2 =
				    replace(
				      [{set_password, password}],
				      Set_password),
				{ok,  Mb} = smerl:add_func(Ma, Get_salt,            true),
				{ok,  Mc} = smerl:add_func(Mb, Get_salted_password, true),
				{ok, _Md} = smerl:add_func(Mc, Set_password2,       true)
			end({ok, M24});
		    [_, _] ->
			{ok, M24}
		end,

	    %%
	    %% Add updated one2many functions
	    %%

	    {Linking_to_me, I_link_to} = model_conf:one2many(Model),

	    {ok, M26} =
		lists:foldl(
		  fun(Other, {ok, Ma}) ->
			  Get_many2one_id2 =
			      replace(
				[{get_many2one_id, id_of(Other)},
				 {field_id, id_of(Other)}
				],
				Get_many2one_id),
			  Get_many2one_instance2 =
			      replace(
				[{get_many2one_instance, Other},
				 {field_id, id_of(Other)},
				 {field, Other}
				],
				Get_many2one_instance),
			  Set_many2one2 =
			      replace(
				[{set_many2one, Other},
				 {field_id, id_of(Other)}
				],
				Set_many2one),
			  {ok,  Mb} = smerl:add_func(Ma, Get_many2one_id2,       true),
			  {ok,  Mc} = smerl:add_func(Mb, Get_many2one_instance2, true),
			  {ok, _Md} = smerl:add_func(Mc, Set_many2one2,          true)
		  end,
		  {ok, M25},
		  I_link_to),

	    {ok, M27} =
		lists:foldl(
		  fun(Other, {ok, Ma}) ->
			  Get_one2many_ids2 =
			      replace(
				[{get_one2many_ids, ids_of(Other)},
				 {field, Other},
				 {model_id, id_of(Model)}
				],
				Get_one2many_ids),
			  Get_one2many_instances2 =
			      replace(
				[{get_one2many_instances, plural(Other)},
				 {field, Other},
				 {model_id, id_of(Model)}
				],
				Get_one2many_instances),
			  Add_one2many2 =
			      replace(
				[{add_one2many, add_of(Other)},
				 {field, Other}
				],
				Add_one2many),
			  %% smerl: 'remove_one2many' => remove_<field>, field => <field>, field_id => <field>_id
			  Remove_one2many2 =
			      replace(
				[{remove_one2many, remove_of(Other)},
				 {field, Other},
				 {field_id, id_of(Other)}
				],
				Remove_one2many),
			  {ok,  Mb} = smerl:add_func(Ma, Get_one2many_ids2,       true),
			  {ok,  Mc} = smerl:add_func(Mb, Get_one2many_instances2, true),
			  {ok,  Md} = smerl:add_func(Mc, Add_one2many2,           true),
			  {ok, _Me} = smerl:add_func(Md, Remove_one2many2,        true)
		  end,
		  {ok, M26},
		  Linking_to_me),

	    %%
	    %% Add updated many2many functions
	    %%

	    {ok, M28} =
		lists:foldl(
		  fun(Other, {ok, Ma}) ->
			  Get_many2many_ids2 =
			      replace(
				[{get_many2many_ids, ids_of(Other)},
				 {model, Model},
				 {field, Other}
				],
				Get_many2many_ids),
			  Get_many2many_instances2 =
			      replace(
			  	[{get_many2many_instances, plural(Other)},
			  	 {get_many2many_ids, ids_of(Other)},
			  	 {field, Other}
			  	],
			  	Get_many2many_instances),
			  Add_many2many2 =
			      replace(
			  	[{add_many2many, add_of(Other)},
			  	 {model, Model},
			  	 {field, Other}
			  	],
			  	Add_many2many),
			  %% smerl: 'remove_many2many' => remove_<field>, 'model' => <model>, 'field' => <field>
			  Remove_many2many2 =
			      replace(
			  	[{remove_many2many, remove_of(Other)},
			  	 {model, Model},
			  	 {field, Other}
			  	],
			  	Remove_many2many),
			  {ok,  Mb} = smerl:add_func(Ma, Get_many2many_ids2,       true),
			  {ok,  Mc} = smerl:add_func(Mb, Get_many2many_instances2, true),
			  {ok,  Md} = smerl:add_func(Mc, Add_many2many2,           true),
			  {ok, _Me} = smerl:add_func(Md, Remove_many2many2,        true)
		  end,
		  {ok, M27},
		  model_conf:many2many(Model)),

	    %%
	    %% Compile updated module
	    %%

	    ok = smerl:compile(M28),

	    %%
	    %% Return module name
	    %%

	    Module
    end.

module(Model) ->
    to:atom("model_module_" ++ to:string(Model)).

new(Model, Context) ->
    Module = module(Model),
    Module:new(Context).

%%
%% Internal
%%

abstract_list([H | T], Row) when is_atom(H) ->
    {cons, Row, {atom, Row, H}, abstract_list(T, Row)};
abstract_list([], Row) ->
    {nil, Row}.

id_of(Atom) when is_atom(Atom) ->
    to:atom(to:string(Atom) ++ "_id").

ids_of(Atom) when is_atom(Atom) ->
    to:atom(to:string(Atom) ++ "_ids").

add_of(Atom) ->
    to:atom("add_" ++ to:string(Atom)).

remove_of(Atom) ->
    to:atom("remove_" ++ to:string(Atom)).

plural(Atom) when is_atom(Atom) ->
    to:atom(plural(to:string(Atom)));
plural(Stringable) ->
    case lists:reverse(to:string(Stringable)) of
	"y" ++ R -> lists:reverse(R, "ies");
	"s" ++ R -> lists:reverse(R, "ses");
	"hs" ++ R -> lists:reverse(R, "shes");
	R -> lists:reverse(R, "s")
    end.

replace(Kvs, In) ->
    lists:foldl(fun({K, V}, A) -> replace(K, V, A) end, In, Kvs).

replace(From, To, In) when is_tuple(In) ->
    list_to_tuple(replace(From, To, tuple_to_list(In)));
replace(From, To, [From | In]) ->
    [To | replace(From, To, In)];
replace(From, To, [H | In]) when is_tuple(H) orelse is_list(H) ->
    [replace(From, To, H) | replace(From, To, In)];
replace(From, To, [H | In]) ->
    [H | replace(From, To, In)];
replace(_From, _To, []) ->
    [];
replace(_From, _To, In) ->
    In.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

plural_test_() ->
    [?_assertEqual("fishes",   plural("fish")), %% yes, could be "fish" as well, havinig limited plural rules
     ?_assertEqual("skies",    plural("sky")),
     ?_assertEqual("boats",    plural("boat")),
     ?_assertEqual("accesses", plural("access")),
     ?_assertEqual("aces",     plural("ace"))
    ].

abstract_list_test_() ->
    Line = ?LINE,
    [?_assertEqual({nil, Line},
		   abstract_list([], Line)),
     ?_assertEqual({cons, Line, {atom, Line, a}, {nil, Line}},
		   abstract_list([a], Line)),
     ?_assertEqual({cons, Line, {atom, Line, a}, {cons, Line, {atom, Line, b}, {nil, Line}}},
		   abstract_list([a, b], Line))
    ].

init_1_test_() ->
    [fun() ->
	     meck:new(model_conf),
	     meck:expect(model_conf, many2many, fun(M) -> ?assertEqual(M, Model), Many2many end),
	     meck:expect(model_conf, one2many,  fun(M) -> ?assertEqual(M, Model), One2many  end),
	     meck:expect(model_conf, data,      fun(M) -> ?assertEqual(M, Model), Data      end),
	     meck:expect(model_conf, data,      fun(M) -> ?assertEqual(M, Model), Data      end),
	     meck:expect(model_conf, one2many,  fun(M) -> ?assertEqual(M, Model), One2many  end),
	     meck:expect(model_conf, many2many, fun(M) -> ?assertEqual(M, Model), Many2many end),
	     Module = module(Model),
	     ?assertEqual(Module, (catch init(Model))),
	     meck:validate(model_conf),
	     meck:unload(model_conf),
	     assert_exported_functions(Module, Data, One2many, Many2many)
     end
     || {Model, Data, One2many, Many2many}
	    <- [{ape, [name, salt, salted_password], {[keeper], [banana, apple]}, [tree]}]
    ].

assert_exported_functions(Module, Data, {Linking_to_me, I_link_to} = _One2many, Many2many) ->
    Exports = Module:module_info(exports),
    [case X of
	 X when X =:= salt orelse X =:= salted_password ->
	     ?assertEqual([], [{salt,            1}] -- Exports),
	     ?assertEqual([], [{salted_password, 1}] -- Exports),
	     ?assertEqual([], [{password,        2}] -- Exports);
	 X when X =/= password andalso X =/= new andalso X =/= update andalso X =/= delete ->
	     ?assertEqual([], [{X, 1}] -- Exports),
	     ?assertEqual([], [{X, 2}] -- Exports);
	 X when X =:= password andalso X =:= new andalso X =:= update andalso X =:= delete ->
	     ?assertEqual(not_allowed, X)
     end
     || X <- Data
    ],
    [begin
	 ?assertEqual([], [{ids_of(X),    1}] -- Exports),
	 ?assertEqual([], [{plural(X),    1}] -- Exports),
	 ?assertEqual([], [{add_of(X),    2}] -- Exports),
	 ?assertEqual([], [{remove_of(X), 3}] -- Exports)
     end
     || X <- Linking_to_me
    ],
    [begin
	 ?assertEqual([], [{id_of(X), 1}] -- Exports),
	 ?assertEqual([], [{X,        1}] -- Exports),
	 ?assertEqual([], [{X,        2}] -- Exports)
     end
     || X <- I_link_to
    ],
    [begin
	 ?assertEqual([], [{ids_of(X),    1}] -- Exports),
	 ?assertEqual([], [{plural(X),    1}] -- Exports),
	 ?assertEqual([], [{add_of(X),    2}] -- Exports),
	 ?assertEqual([], [{remove_of(X), 2}] -- Exports)
     end
     || X <- Many2many
    ],
    ok.

-endif.
