-module(model).
-export([create/2,
	 read_by_id/2,
	 read_by_kvs/2,
	 read_by_credentials/2
	]).

-type key() :: atom() | string().
-type value() :: any().
-type kvs() :: [{key(), value()}].
-type credentials() :: kvs().
-type model() :: atom().
-opaque data_instance() :: any().

-spec create(model(), kvs()) -> data_instance() | erlang:throw({invalid, model()}).
create(Model, Kvs) ->
    Id = model_id:generate(),
    Restricted_keys = ["id", "salt", "password", "salted_password"],
    Kvs2 = [Kv || {K, _} = Kv <- Kvs, not lists:member(to:string(K), Restricted_keys)],
    Kvs3 =
	case [to:string(V) || {K, V} <- Kvs, to:string(K) =:= "password"] of
	    [Password | _] ->
		{ok, Salt, Salted_password} = model_password:salt_password(Password),
		[{id, Id},
		 {salt, Salt},
		 {salted_password, Salted_password}
		 | Kvs2
		];
	    [] ->
		[{id, Id} | Kvs2]
	end,
    model_mnesia:create(Model, Kvs3).

-spec read_by_id(model(), model_id:id()) -> data_instance() | erlang:throw({invalid, model()}).
read_by_id(Model, Id) ->
    model_mnesia:read_by_id(Model, Id).

-spec read_by_kvs(model(), kvs()) -> [data_instance()].
read_by_kvs(Model, Kvs) ->
    Query = model_mnesia:to_query(Model, Kvs),
    model_mnesia:read_by_query(Model, Query).

-spec read_by_credentials(model(), kvs()) -> data_instance() | erlang:throw({invalid, model()}).
read_by_credentials(Model, Credentials) ->
    case authentication_method(Credentials) of
	"basic" ->
	    case basic_credentials(Credentials) of
		{none, _} -> throw({invalid, Model});
		{_, none} -> throw({invalid, Model});
		{Id, Password} ->
		    Instance = read_by_id(Model, Id),
		    {ok, _Salt, Salted_password} =
			model_password:salt_password(Password, Instance:salt()),
		    case Instance:salted_password() =:= Salted_password of
			true ->
			    Instance;
			false ->
			    throw({invalid, Model})
		    end
	    end
    end.

-spec authentication_method(credentials()) -> string().
authentication_method(Credentials) ->
    case lists:dropwhile(fun({K, _V}) -> "authentication_method" =/= to:string(K) end, Credentials) of
	[{_, V} | _] ->
	    to:lower(to:string(V));
	[] ->
	    throw({invalid, credentials})
    end.

-spec basic_credentials(credentials()) -> {Id::string(), Password::string()}.
basic_credentials(Credentials) ->
    basic_credentials(Credentials, none, none).

basic_credentials([{K, V} | T], Id, Password) when Id =:= none orelse Password =:= none ->
    case to:string(K) of
	"id" when Id =:= none ->
	    basic_credentials(T, to:string(V), Password);
	"password" when Password =:= none ->
	    basic_credentials(T, Id, to:string(V));
	Key when Key =/= "id" andalso Key =/= "password" ->
	    basic_credentials(T, Id, Password)
    end;
basic_credentials(_, Id, Password) ->
    {Id, Password}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

create_test_() ->
    Model = model,
    Id = "id",
    Salt = "salt",
    Password = "password",
    Salted_password = "salted_password",
    Instance = instance,
    Generate = fun() -> Id end,
    Salt_password = fun(P) when P =:= Password -> {ok, Salt, Salted_password} end,
    [fun() ->
	     Create_kvs =
		 [{a, "a"},
		  {salt, "bad_salt"},
		  {password, Password},
		  {b, 7},
		  {id, "bad_id"},
		  {salted_password, "bad_salted_password"},
		  {c, c}
		 ],
	     Expected_kvs =
		 [{id, Id},
		  {salt, Salt},
		  {salted_password, Salted_password},
		  {a, "a"},
		  {b, 7},
		  {c, c}
		 ],
	     Create =
		 fun(M, Kvs) ->
			 ?assertEqual(M, Model),
			 ?assertEqual(Expected_kvs, Kvs),
			 Instance
		 end,
	     meck:new(model_id),
	     meck:new(model_password),
	     meck:new(model_mnesia),
	     meck:expect(model_id, generate, Generate),
	     meck:expect(model_password, salt_password, Salt_password),
 	     meck:expect(model_mnesia, create, Create),
	     ?assertEqual(Instance, create(Model, Create_kvs)),
	     ?assert(meck:validate(model_id)),
	     ?assert(meck:validate(model_password)),
	     ?assert(meck:validate(model_mnesia)),
	     meck:unload(model_id),
	     meck:unload(model_password),
	     meck:unload(model_mnesia)
     end,
     fun() ->
	     Create_kvs =
		 [{a, "a"},
		  {salt, "bad_salt"},
		  {id, "bad_id"},
		  {b, 7},
		  {salted_password, "bad_salted_password"},
		  {c, c}
		 ],
	     Expected_kvs =
		 [{id, Id},
		  {a, "a"},
		  {b, 7},
		  {c, c}
		 ],
	     Create = fun(M, Kvs) -> ?assert(M =:= Model andalso Kvs =:= Expected_kvs), Instance end,
	     meck:new(model_id),
	     meck:new(model_password),
	     meck:new(model_mnesia),
	     meck:expect(model_id, generate, Generate),
	     meck:expect(model_mnesia, create, Create),
	     ?assertEqual(Instance, create(Model, Create_kvs)),
	     ?assert(meck:validate(model_id)),
	     ?assert(meck:validate(model_password)),
	     ?assert(meck:validate(model_mnesia)),
	     meck:unload(model_id),
	     meck:unload(model_password),
	     meck:unload(model_mnesia)
     end
    ].

read_by_id_test_() ->
    [fun() ->
	     meck:new(model_mnesia),
	     meck:expect(model_mnesia,
			 read_by_id,
			 fun(M, I) ->
				 ?assertEqual(M, Model),
				 ?assertEqual(I, Id),
				 x
			 end),
	     ?assertEqual(x, read_by_id(Model, Id)),
	     meck:validate(model_mnesia),
	     meck:unload(model_mnesia)
     end
    || {Model, Id} <- [{car, 7}, {plane, 747}]
    ].

read_by_kvs_test_() ->
    [fun() ->
	     meck:new(model_mnesia),
	     meck:expect(model_mnesia,
			 to_query,
			 fun(M, K) ->
				 ?assertEqual(M, Model),
				 ?assertEqual(K, Kvs),
				 Query
			 end),
	     meck:expect(model_mnesia,
			 read_by_query,
			 fun(M, Q) ->
				 ?assertEqual(M, Model),
				 ?assertEqual(Q, Query),
				 [x]
			 end),
	     ?assertEqual([x], read_by_kvs(Model, Kvs)),
	     meck:validate(model_mnesia),
	     meck:unload(model_mnesia)
     end
    || {Model, Kvs, Query} <- [{car, [], empty_query}, {plane, [{a,a}], a}]
    ].

read_by_credentials_test_() ->
    [fun() ->
	     Salt = salt,
	     meck:new(model_instance),
	     meck:new(model_mnesia),
	     meck:new(model_password),
	     meck:expect(model_mnesia,
			 read_by_id,
			 fun(M, I) ->
				 ?assertEqual(Model, M),
				 ?assertEqual(Id, I),
				 model_instance
			 end),
	     meck:expect(model_instance, salt, fun() -> Salt end),
	     meck:expect(model_password,
			 salt_password,
			 fun(P, S) ->
				 ?assertEqual(Password, P),
				 ?assertEqual(Salt, S),
				 {ok, S, Salted_password}
			 end),
	     meck:expect(model_instance,
			 salted_password,
			 fun() ->
				 Salted_password
			 end),
	     Credentials =
		 [{"authentication_method", "basic"},
		  {"id", Id},
		  {"password", Password}
		 ],
	     ?assertEqual(model_instance, (catch read_by_credentials(Model, Credentials))),
	     meck:validate(model_instance),
	     meck:validate(model_mnesia),
	     meck:validate(model_password),
	     meck:unload(model_instance),
	     meck:unload(model_mnesia),
	     meck:unload(model_password)
     end
    || {Model, Id, Password, Salted_password} <-
	   [{car,   "123", "password", "salted_password"},
	    {plane, "747", "fasten", "seat_belts"}
	   ]
    ].

-endif.
