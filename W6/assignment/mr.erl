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


stop(Pid) -> Pid ! {self(), stop}. % should probably handle errors

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
    ReducerPid = spawn(fun() -> reducer_loop(-1, N, fun(X, Acc) -> X+Acc end, 0) end),
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
        Reducer ! {self(), state, {length(Data), RedFun, RedInit}},
        send_func(Mappers, MapFun),
        send_data(Mappers, Data),
        receive
            {Reducer, ok, Result} -> reply_ok(From, Result)
        end;

	Unknown ->
	    io:format("[COORDINATOR] unknown message: ~p~n",[Unknown]), 
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

reducer_loop(Parent, N, Fun, Init) -> % N and Fun added
    receive
	stop -> 
	    io:format("Reducer ~p stopping~n", [self()]),
	    ok;
	
	{From, state, {M, Func, Initial}} ->
        reducer_loop(From, M, Func, Initial);
    
    {_, data, Data} ->
        Acc = spawn(gather_data_from_mappers(Fun, Init, N-1)),
        Result = Fun(Data, Acc),
        reply_ok(Parent, Result),
        reducer_loop(Parent, N, Fun, Init);

	Unknown ->
	    io:format("[REDUCER] unknown message: ~p~n",[Unknown]), 
        reducer_loop(Parent, N, Fun, Init)
    
    end.

gather_data_from_mappers(Fun, Acc, Missing) ->
    receive
    {_, data, Data} ->
        Acc2 = Fun(Data, Acc),
        Result = gather_data_from_mappers(Fun, Acc2, Missing-1)
    end,
    io:format("received data"),
    Result.


%%% Mapper

mapper_loop(Reducer, Fun, Data) -> % Data added
    receive
	stop -> 
	    io:format("Mapper ~p stopping~n", [self()]),
	    ok;
    
    {data, Datum} ->
        Result = Fun(Datum),
        Reducer ! {self(), data, Result},
        mapper_loop(Reducer, Fun, Datum);
	
	{_, func, Func} ->
        mapper_loop(Reducer, Func, Data);

	Unknown ->
	    io:format("[MAPPER] unknown message: ~p~n",[Unknown]), 
	    mapper_loop(Reducer, Fun, Data)
    end.
