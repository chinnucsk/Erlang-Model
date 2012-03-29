-module(model_conf).
-export([data/0,
	 data/1,
	 one2many/0,
	 one2many/1,
	 many2many/0,
	 many2many/1
	]).

-type model() :: atom().
-type field() :: atom().

-spec data() -> [{model(), [field(), ...]}].
data() ->
    {ok, Data} = application:get_env(model, data),
    Data.

-spec data(model()) -> [field(), ...].
data(Model) ->
    {value, {Model, Data}} = lists:keysearch(Model, 1, data()),
    Data.

%% one = the one being pointed too, many = the one pointing out the one
-spec one2many() -> [{Linked_to::model(), Linked_by::model()}].
one2many() ->
    {ok, One2many} = application:get_env(model, one2many),
    One2many.

-spec one2many(model()) -> {[Linking_to_me::model()], [I_link_to::model()]}.
one2many(Model) ->
    {[M || {O, M} <- one2many(), O =:= Model],
     [O || {O, M} <- one2many(), M =:= Model]
    }.

-spec many2many() -> [{model(), model()}].
many2many() ->
    {ok, Many2many} = application:get_env(model, many2many),
    Many2many.

-spec many2many(model()) -> [model()].
many2many(Model) ->
    lists:usort(
      [M || {M, O} <- many2many(), O =:= Model] ++
      [O || {O, M} <- many2many(), M =:= Model]).
