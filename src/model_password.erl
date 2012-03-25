-module(model_password).
-export([generate/0,
	 generate_salt/0,
	 salt_password/1,
	 salt_password/2
	]).

-type password() :: string().
-type salt() :: string().
-type salted_password() :: string().

-spec generate() -> password().
generate() ->
    Allowed_characters = lists:seq($a, $z) ++ lists:seq($A, $Z),
    Max = length(Allowed_characters),
    {ok, Length} = application:get_env(oauth2, length_of_generated_password),
    for(Length, fun() -> random(Max) end).

for(N, Fun) when N > 1 -> [Fun() | for(N - 1, Fun)];
for(0, _Fun) -> [].

random(Max) ->
    {Ms, S, Us} = erlang:now(),
    random:seed(Ms, S, Us),
    random:uniform(Max).

-spec generate_salt() -> salt().
generate_salt() ->
    "16#" ++ Salt = lists:flatten(io_lib:fwrite("~.16x", [16#FFFFFFFF, "16#"])),
    Salt.

-spec salt_password(password()) -> {ok, salt(), salted_password()}.
salt_password(Password) ->
    salt_password(Password, generate_salt()).

-spec salt_password(password(), salt()) -> {ok, salt(), salted_password()}.
salt_password(Password, Salt) ->
    Hash = list_to_binary(Password ++ Salt),
    {ok, Salt, crypto:sha(Hash)}.
