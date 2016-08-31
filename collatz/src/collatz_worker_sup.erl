-module(collatz_worker_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Children = [
        #{
        	id => collatz_worker,
        	start => {collatz_worker, start_link, []},
        	restart => temporary
        }
	],
	Flags = #{
		strategy => simple_one_for_one,
		intensity => 0,
		period => 1
	},
	{ok, {Flags, Children}}.
