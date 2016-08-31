-module(collatz_server).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([count/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
	current = 1,
	last,
	limit,
	workers = #{},
	result = {0, 0},
	reply
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec count(integer(), integer()) -> {integer(), integer()}.
count(Start, Stop) ->
	ok = gen_server:cast(?MODULE, {start, Start, Stop, self()}),
	receive
		{collatz_result, Result} ->
			Result
	end.

%% gen_server.

init([]) ->
	{ok, Limit} = application:get_env(collatz, workers),
	{ok, #state{limit = Limit}}.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({start, Start, Stop, ReplyTo}, State) ->
	self() ! spawn_worker,
	{noreply, State#state{current = Start, last = Stop, reply = ReplyTo}};
handle_cast({result, X, N}, #state{result = {_OldX, OldN}} = State) when N > OldN ->
	{noreply, State#state{result = {X, N}}};
handle_cast({result, _X, _N}, State) ->
	{noreply, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(spawn_worker, #state{current = Current, last = Last} = State) when Current > Last ->
	{noreply, State};
handle_info(spawn_worker, #state{limit = Limit, workers = Workers} = State) when map_size(Workers) >= Limit ->
	{noreply, State};
handle_info(spawn_worker, #state{current = Current, workers = Workers} = State) ->
	Worker = spawn_worker(Current),
	self() ! spawn_worker,
	{noreply, State#state{current = Current + 1, workers = Workers#{Worker => Current}}};
handle_info({'DOWN', _Ref, process, _Pid, normal}, #state{workers = Workers, current = Current, last = Last} = State)
when map_size(Workers) =:= 1, Current > Last ->
	#state{reply = Reply, result = Result} = State,
	Reply ! {collatz_result, Result},
	{noreply, State#state{workers = #{}}};
handle_info({'DOWN', Ref, process, Pid, normal}, #state{workers = Workers} = State) ->
	self() ! spawn_worker,
	{noreply, State#state{workers = maps:remove({Pid, Ref}, Workers)}};
handle_info({'DOWN', Ref, process, Pid, _Reason}, #state{workers = Workers} = State) ->
	DeadWorker = {Pid, Ref},
	#{DeadWorker := X} = Workers,
	Worker = spawn_worker(X),
	{noreply, State#state{workers = maps:remove({Pid, Ref}, Workers#{Worker => X})}};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


spawn_worker(X) ->
	{ok, WorkerPid} = supervisor:start_child(collatz_worker_sup, [X]),
	Monitor = erlang:monitor(process, WorkerPid),
	{WorkerPid, Monitor}.
