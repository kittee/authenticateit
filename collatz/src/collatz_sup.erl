-module(collatz_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Children = [
		#{
			id => collatz_server,
			start => {collatz_server, start_link, []}
		},
		#{
			id => collatz_worker_sup,
			start => {collatz_worker_sup, start_link, []},
			type => supervisor
		}
	],
	Flags = #{
		strategy => one_for_all,
		intensity => 0,
		period => 1
	},
	{ok, {Flags, Children}}.
