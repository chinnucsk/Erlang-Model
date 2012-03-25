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
    [{client,		[email, salt, salted_password, default_redirect_uri, default_scope, type]},
     {field_access,	[crud]},
     {filter_access,	[condition]},
     {redirect_uri,	[uri]},
     {resource_server,	[email, salt, salted_password]},
     {resource_type,	[name]},
     {resource_field,	[name]},
     {resource_filter,	[name]},
     {scope,		[name, description]},
     {session,		[state]},
     {user,		[email, salt, salted_password]}
    ].

-spec data(model()) -> [field(), ...].
data(Model) ->
    {value, {Model, Data}} = lists:keysearch(Model, 1, data()),
    Data.

%% one = the one being pointed too, many = the one pointing out the one
-spec one2many() -> [{Linked_to::model(), Linked_by::model()}].
one2many() ->
    [{client,		token_data},
     {client,		redirect_uri},
     {grant,		client},
     {grant,		scope},
     {grant,		user},
     {redirect_uri,	authorization_code},
     {redirect_uri,	session},
     {resource_server,	resource_type},
     {resource_type,	field_access},
     {resource_type,	filter_access},
     {resource_type,	resource_field},
     {resource_type,	resource_filter},
     {scope,		field_access},
     {scope,		filter_access},
     {token_data,	access_token},
     {token_data,	authorization_code},
     {token_data,	refresh_token},
     {token_data,	session},
     {user,		token_data}
    ].

-spec one2many(model()) -> {[Linking_to_me::model()], [I_link_to::model()]}.
one2many(Model) ->
    {[M || {O, M} <- one2many(), O =:= Model],
     [O || {O, M} <- one2many(), M =:= Model]
    }.

-spec many2many() -> [{model(), model()}].
many2many() ->
    [{token_data,	scope}
    ].

-spec many2many(model()) -> [model()].
many2many(Model) ->
    lists:usort(
      [M || {M, O} <- many2many(), O =:= Model] ++
      [O || {O, M} <- many2many(), M =:= Model]).


%% info() ->
%%     [{access_token,       [id, client_id, user_id, scope_ids, refresh_token_id]},
%%      {authorization_code, [id, client_id, user_id, redirect_uri_id, scope_ids]},
%%      {client,             [id, salt, salted_password, default_redirect_uri, default_scope, type]},
%%      {grant,              [id, client_id, user_id, scope_id]},
%%      {redirect_uri,       [id, client_id, uri]},
%%      {refresh_token,      [id, client_id, user_id, scope_ids]},
%%      {resource_server,    [id, salt, salted_password]},
%%      {resource_type,      [id, resource_server_id, name]},
%%      {resource_field,     [id, resource_type_id, name]},
%%      {resource_filter,    [id, resource_type_id, name]},
%%      {scope,              [id, name, description]},
%%      {session,            [id, client_id, user_id, redirect_uri_id, scope_ids, state, response_type]},
%%      {user,               [id, salt, salted_password]}
%%     ].

%% model() ->
%%     [%% resource_server
%%      {resource_server,        [salt, salted_password, {many, resource_type}]},
%%      {resource_type,          [name, {many, resource_field}, {one, resource_filter}]},
%%      {resource_type_access,   [access, {one, resource_type}, {many, scope}]},
%%      {resource_field,         [name]},
%%      {resource_field_access,  [access, {one, resource_field}, {many, scope}]},
%%      {resource_filter,        [name, {many, resource_parameter}]},
%%      {resource_filter_access, [access, {one, resource_filter}, {many, scope}]},
%%      {resource_parameter,     [name]},

%%      %% client
%%      {client,                 [salt, salted_password, default_redirect_uri, default_scope, type, {many, redirect_uri}, {many, scope}]},
%%      {redirect_uri,           [uri]},

%%      %% scope
%%      {scope,                  [name, description, {many, resource_type_access}, {many, resource_field_access}, {many, resource_filter_access}]},
%%      {scope_change_log,       [scope, change, timestamp, who, reason]},

%%      %% tokens
%%      {session,                [response_type, state, {one, client}, {one, redirect_uri}, {one, user}, {many, scope}]},
%%      {authorization_code,     [{one, client}, {one, user}, {many, scope}]},
%%      {refresh_token,          [{one, client}, {one, user}, {many, scope}]},
%%      {access_token,           [{one, client}, {one, user}, {many, scope}]}
%%     ].

%% record_fields(Model) ->
%%     [_|_] = Spec = kvs:value(Model, model()),
%%     From = [{M, from, R} || {R, M} <- Spec],
%%     To = [{M, to, R} || {M, S} <- model(), {R, Mdl} <- S, Mdl =:= Model],
%%     lists:sort(From ++ To),
%%     [id | Spec].

%% %% many2many => index table
%% %% many2one  => ref in "one"
%% %% one2many  => ref in "one"
%% %% one2one   => ref in both "one"

%% local([{M, from, many}, {M, to, many} | T]) -> local(T);
%% local([{M, from, many}, {M, to, one } | T]) -> local(T);
%% local([{_, from, many}                | T]) -> local(T);
%% local([{M, from, one},  {M, to, many} | T]) -> [id_of(M) | local(T)];
%% local([{M, from, one},  {M, to, one}  | T]) -> [id_of(M) | local(T)];
%% local([{M, from, one}                 | T]) -> [id_of(M) | local(T)];
%% local([]) -> [].

%% index([{M, from, many}, {M, to, many} | T]) -> [M | index(T)];
%% index([{M, from, many}, {M, to, one } | T]) -> index(T);
%% index([{_, from, many}                | T]) -> index(T);
%% index([{M, from, one},  {M, to, many} | T]) -> index(T);
%% index([{M, from, one},  {M, to, one}  | T]) -> index(T);
%% index([{_, from, one}                 | T]) -> index(T);
%% index([]) -> [].

%% id_of(M) ->
%%     to:atom(to:striong(M) ++ "_id").
