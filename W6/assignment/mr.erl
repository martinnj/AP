%%%-------------------------------------------------------------------
%%% @author Ken Friis Larsen <kflarsen@diku.dk>
%%% @copyright (C) 2011, Ken Friis Larsen
%%% Created : Oct 2011 by Ken Friis Larsen <kflarsen@diku.dk>
%%%-------------------------------------------------------------------

-module(mr).

-export([start/1, stop/1, job/5]).

%%%% Interface

start(N) ->
    {Reducer, Mappers} = init(N),
    {ok, spawn(fun() -> coordinator_loop(Reducer, Mappers) end)}.


stop(Pid) -> Pid ! {self(), stop}.

job(CPid, MapFun, RedFun, RedInit, Data) ->
    rpc(CPid, {job, MapFun, RedFun, RedInit, Data}).


%%%% Internal implementation

init_mappers(L, R, N) ->
    case N of
        0 -> L;
        M -> MapperPid = spawn(fun() -> mapper_loop(R, fun(x) -> x end, []) end),
             init_mappers([MapperPid | L], R, M-1)
    end.
    

init(N) ->
    ReducerPid = spawn(fun() -> reducer_loop() end),
    MapperPids = init_mappers([], ReducerPid, N),
    {ReducerPid, MapperPids}.


%% synchronous communication

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
	{Pid, Response} ->
	    Response
    end.

reply(From, Msg) -> From ! {self(), Msg}.
reply_ok(From) -> reply(From, ok).
reply_ok(From, Msg) -> reply(From, {ok, Msg}).


%% asynchronous communication

async(Pid, Msg) -> Pid ! Msg.
stop_async(Pid) -> async(Pid, stop).
data_async(Pid, D) -> async(Pid, {data, D}).


%%% Coordinator

coordinator_loop(Reducer, Mappers) ->
    receive
	{From, stop} ->
	    io:format("~p stopping~n", [self()]),
	    lists:foreach(fun stop_async/1, Mappers),
	    stop_async(Reducer),
	    reply_ok(From);

    {From, {job, MapFun, RedFun, RedInit, Data}} ->
        io:format("starting job~n"),
        send_func(Mappers, MapFun),
        send_data(Mappers, Data),
        case rpc(Reducer, {job, {RedFun, RedInit, length(Data)}}) of
            {ok, Result} -> reply_ok(From, Result),
                            io:format("Result: ~p~n", [Result]);
            {error, Reason} -> From ! {error, Reason} % unused
        end,
        coordinator_loop(Reducer, Mappers);

	Unknown ->
	    io:format("unknown message in ~p: ~p~n",[self(),Unknown]), 
        coordinator_loop(Reducer, Mappers)
    end.

% sends a function to the mappers
send_func(Mappers, Func) ->
    send_func_loop(Mappers, Func).

send_func_loop([], _) -> ok;
send_func_loop([H | T], Func) ->
    H ! {self(), func, Func},
    send_func_loop(T, Func).

% sends data to the mappers
send_data(Mappers, Data) ->
    send_loop(Mappers, Mappers, Data).

send_loop(Mappers, [Mid|Queue], [D|Data]) ->
    data_async(Mid, D),
    send_loop(Mappers, Queue, Data);
send_loop(_, _, []) -> ok;
send_loop(Mappers, [], Data) ->
    send_loop(Mappers, Mappers, Data).


%%% Reducer

reducer_loop() ->
    receive
	stop -> 
	    io:format("Reducer ~p stopping~n", [self()]),
	    ok;
    
    {From, {job, {Fun, Init, Missing}}} ->
        % pass control of thread to gather_data_from_mappers
        reply_ok(From, gather_data_from_mappers(Fun, Init, Missing)),
        reducer_loop();
    
	Unknown ->
	    io:format("unknown message in ~p: ~p~n",[self(),Unknown]), 
        reducer_loop()
    
    end.

gather_data_from_mappers(Fun, Acc, Missing) ->
    receive
        {_, data, Data} ->
            Acc2 = Fun(Data, Acc)
    end,
    
    case (Missing > 1) of
        true  -> gather_data_from_mappers(Fun, Acc2, Missing - 1);
        false -> Acc2
    end.


%%% Mapper

mapper_loop(Reducer, Fun, Data) -> % Data added
    receive
	stop -> 
	    io:format("Mapper ~p stopping~n", [self()]),
	    ok;
    
    {data, Datum} ->
        Reducer ! {self(), data, Fun(Datum)},
        mapper_loop(Reducer, Fun, Datum);
	
	{_, func, Func} ->
        mapper_loop(Reducer, Func, Data);

	Unknown ->
	    io:format("unknown message in ~p: ~p~n",[self(),Unknown]), 
	    mapper_loop(Reducer, Fun, Data)
    end.
