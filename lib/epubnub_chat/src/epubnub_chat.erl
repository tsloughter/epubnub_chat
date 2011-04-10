%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>
%%% @copyright (C) 2011, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 10 Apr 2011 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(epubnub_chat).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2, stop/1, message/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export_type([]).

-define(SERVER, ?MODULE).

-record(state, {pid, epn, channel}).

%%%===================================================================
%%% Public Types
%%%===================================================================

%%%===================================================================
%%% API
%%%===================================================================

start_link(Channel) ->
    start_link(epubnub:new(), Channel).

start_link(EPN, Channel) ->
    gen_server:start_link(?MODULE, [EPN, Channel], []).

message(Server, Msg) ->
    gen_server:cast(Server, {message, Msg}).

stop(Server) ->
    gen_server:cast(Server, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([EPN, Channel]) ->
    {ok, PID} = epubnub_sup:subscribe(EPN, Channel, self()),
    {ok, #state{epn=EPN, pid=PID, channel=Channel}}.

%% @private
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast({message, Msg}, State=#state{epn=EPN, channel=Channel}) ->
    epubnub:publish(EPN, Channel, Msg),
    {noreply, State}.

%% @private
handle_info({message, Message}, State) ->
    io:format("~p~n", [Message]),
    {noreply, State}.

%% @private
terminate(_Reason, #state{pid=PID}) ->
    epubnub:unsubscribe(PID),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
