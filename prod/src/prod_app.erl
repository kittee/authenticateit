-module(prod_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
        {'_', [{"/capture", prod_handler, []}]}
    ]),
    cowboy:start_http(prod_http_listener, 100, [{port, 8080}],
        [{env, [{dispatch, Dispatch}]}]
    ),
	prod_sup:start_link().

stop(_State) ->
	ok.
