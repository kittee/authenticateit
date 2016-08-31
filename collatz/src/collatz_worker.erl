-module(collatz_worker).
-behaviour(gen_server).

%% API.
-export([start_link/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
	x
}).

%% API.

-spec start_link(integer()) -> {ok, pid()}.
start_link(X) ->
	gen_server:start_link(?MODULE, [X], []).

%% gen_server.

init([X]) ->
	self() ! start,
	{ok, #state{x = X}}.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(start, #state{x = X} = State) ->
	N = count(X),
	gen_server:cast(collatz_server, {result, X, N}),
	{stop, normal, State};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



count(X) ->
	count(X, 1).

count(1, N) ->
	N;
count(X, N) when X rem 2 =:= 0 ->
	count(X div 2, N + 1);
count(X, N) ->
	count(X * 3 + 1, N + 1).
