% @Author: weiyanguang
% @Email:  rondsny@gmail.com
% @Date:   2016-11-16 10:57:57
% @Last Modified by:   weiyanguang
% @Last Modified time: 2016-11-16 15:13:37
% @Desc:

-module(net_common_svr).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports

-export([
    start/0,
    start_link/0
    ]).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    start_link().

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
    Result :: {ok, State}
            | {ok, State, Timeout}
            | {ok, State, hibernate}
            | {stop, Reason :: term()}
            | ignore,
    State :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
    {ok, []}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Msg :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
    Result :: {reply, Reply, NewState}
            | {reply, Reply, NewState, Timeout}
            | {reply, Reply, NewState, hibernate}
            | {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason, Reply, NewState}
            | {stop, Reason, NewState},
    Reply :: term(),
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity,
    Reason :: term().
%% ====================================================================
handle_call(Msg, From, State) ->
    try
        do_call(Msg, From, State)
    catch
        Err:Reason ->
            io:format("[~p] do_call error! Msg = ~p, From = ~p, Err = ~p, Reason = ~p~n", [?MODULE, Msg, From, Err, Reason]),
            {reply, error, State}
    end.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Msg :: term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(Msg, State) ->
    try
        do_cast(Msg, State)
    catch
        Err:Reason ->
            io:format("[~p] do_cast error! Msg = ~p, Err = ~p, Reason = ~p~n", [?MODULE, Msg, Err, Reason]),
            {noreply, State}
    end.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Msg :: timeout | term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(Msg, State) ->
    try
        do_info(Msg, State)
    catch
        Err:Reason ->
            io:format("[~p] do_info error! Msg = ~p, Err = ~p, Reason = ~p~n", [?MODULE, Msg, Err, Reason]),
            {noreply, State}
    end.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
    Reason :: normal
            | shutdown
            | {shutdown, term()}
            | term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
    Result :: {ok, NewState :: term()} | {error, Reason :: term()},
    OldVsn :: Vsn | {down, Vsn},
    Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


do_call(_Msg, _From, State) ->
    throw({nomatch, do_call, _Msg}),
    {reply, error, State}.


do_cast({login, Pid}, State) ->
    State2 =
        case lists:member(Pid, State) of
            true -> State;
            _ -> [Pid|State]
        end,
    {noreply, State2};

do_cast({offline, Pid}, State) ->
    State2 = lists:delete(Pid, State),
    {noreply, State2};

do_cast({bcast, BinData}, State) ->
    [gen_server:cast(Pid, {send, BinData})||Pid <-State],
    {noreply, State};

do_cast(_Msg, State) ->
    throw({nomatch, do_cast, _Msg}),
    {noreply, State}.


do_info(_Msg, State) ->
    throw({nomatch, do_info, _Msg}),
    {noreply, State}.