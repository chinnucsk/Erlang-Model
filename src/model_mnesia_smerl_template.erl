%% smerl: 'model_mnesia_smerl_template' => model_module:name(Model)
-module(model_mnesia_smerl_template, [Context]).
-export(
   [add_many2many/1,
    add_one2many/1,
    delete/0,
    id_as_string/0,
    get_data/0,
    get_many2many_ids/0,
    get_many2many_instances/0,
    get_many2one_id/0,
    get_many2one_instance/0,
    get_one2many_ids/0,
    get_one2many_instances/0,
    new/1,
    remove_many2many/1,
    remove_one2many/2,
    set_data/1,
    set_many2one/1,
    set_password/1,
    to_kvs/0,
    update/0
   ]).

%% smerl: no 'new' <field>
new(New_context) ->
    case lists:all(fun({_, _}) -> true; (_) -> false end, New_context) of
	true ->
	    instance(New_context);
	false ->
	    error({?MODULE, new, [New_context]})
    end.

to_kvs() ->
    Context.

%% smerl: 'model' => Model
%% smerl: no 'update' <field>
update() ->
    ok = model_mnesia:update(model, Context).

%% smerl: 'model' => <model>
%% smerl: no 'delete' <field>
%% smerl: 'many2many_fields' => [<field>] where there exist a many2many relationship
%% smerl: 'one2many_fields' => [<field>] where there exist a one2many relationship (others point to me)
%% smerl: 'model_id' => <model>_id
delete() ->
    %% assert no one2many exists refering to this instance
    One2many_fields = one2many_fields,
    Kvs = [{model_id, orddict:fetch(id, Context)}],
    [{_, []} = {Field, model_mnesia:read_by_kvs(Field, Kvs)} || Field <- One2many_fields],
    %% remove all many2many references to this instance
    Many2many_fields = many2many_fields,
    Id = orddict:fetch(id, Context),
    [ok = model_mnesia:delete_all_index(model, Field, Id) || Field <- Many2many_fields],
    %% remove this instance
    ok = model_mnesia:delete(model, orddict:fetch(id, Context)).

%%%
%%% Data (i.e. everything but links
%%%

id_as_string() ->
    model_id:to_string(orddict:fetch(id, Context)).

%% smerl: 'get_data' => <field>, 'field' => <field>
get_data() ->
    orddict:fetch(field, Context).

%% smerl: 'set_data' => <field>, 'field' => <field>
set_data(Value) ->
    Context2 = orddict:store(field, Value, Context),
    Instance = instance(Context2),
    Instance:update(),
    Instance.

%% smerl: add salt/0 and salted_password/0
%% smerl: no salt/1,  salted_password/1, password/0
set_password(Password) ->
    {ok, Salt, Salted_password} = model_password:salted_password(Password),
    Context2 = orddict:store(salt, Salt, Context),
    Context3 = orddict:store(salted_password, Salted_password, Context2),
    Instance = instance(Context3),
    Instance:update(),
    Instance.

%% smerl: 'get_one2many_ids' => <field>_ids, 'field' = <field>, 'model_id' => <model>_id
get_one2many_ids() ->
    model_mnesia:read_ids_by_kvs(field, [{model_id, orddict:fetch(id, Context)}]).

%% smerl: 'get_one2many_instances' => <field>s, 'field' = <field>, 'model_id' => <model>_id
get_one2many_instances() ->
    model:read_by_kvs(field, [{model_id, orddict:fetch(id, Context)}]).

%% smerl: 'add_one2many' => add_<field>, 'field' => <field>
add_one2many(Other_id) when is_integer(Other_id) ->
    Other = model:read_by_id(field, Other_id),
    add_one2many(Other);
add_one2many(Other) ->
    Other:field_id(orddict:fetch(id, Context)),
    Other:update(),
    instance(Context).

%% smerl: 'remove_one2many' => remove_<field>, field => <field>, field_id => <field>_id
remove_one2many(Other_id, Id_replacement) when is_integer(Other_id) ->
    Other = model:read_by_id(field, Other_id),
    remove_one2many(Other, Id_replacement);
remove_one2many(Other, Id_replacement) ->
    Other:field_id(Id_replacement),
    Other:update(),
    instance(Context).

%% smerl: 'get_many2one_id' => <field>_id, 'field_id' => <field>_id
get_many2one_id() ->
    orddict:fetch(field_id, Context).

%% smerl: 'get_many2one_instance' => <field>, 'field' => <field>, 'field_id' => <field>_id
get_many2one_instance() ->
    model:read_by_id(field, orddict:fetch(field_id, Context)).

%% smerl: 'set_many2one' => <field>, 'field_id' => <field>_id
set_many2one(Other_id) when is_integer(Other_id)  ->
    Context2 = orddict:store(field_id, Other_id, Context),
    Instance = instance(Context2),
    Instance:update(),
    Instance;
set_many2one(Other) ->
    set_many2one(Other:id()).

%% smerl: 'get_many2many_ids' => <field>_ids, 'model' => <model>, 'field' => <field>
get_many2many_ids() ->
    model_mnesia:read_index_ids(model, field, orddict:fetch(id, Context)).

%% smerl: 'get_many2many_instances' => <field>s, 'get_many2many_ids' => <field>_ids, 'field' => <field>
get_many2many_instances() ->
    [model:read_by_id(field, Id) || Id <- get_many2many_ids()].

%% smerl: 'add_many2many' => add_<field>, 'model' => <model>, 'field' => <field>
add_many2many(Other_id) when is_integer(Other_id) ->
    ok = model_mnesia:write_index(model, field, orddict:fetch(id, Context), Other_id),
    instance(Context);
add_many2many(Other) ->
    add_many2many(Other:id()).

%% smerl: 'remove_many2many' => remove_<field>, 'model' => <model>, 'field' => <field>
remove_many2many(Other_id) when is_integer(Other_id) ->
    ok = model_mnesia:delete_index(model, field, orddict:fetch(id, Context), Other_id),
    instance(Context);
remove_many2many(Other) ->
    remove_many2many(Other:id()).
