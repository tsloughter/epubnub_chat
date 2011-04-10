%%%----------------------------------------------------------------
%%% @author  Tristan Sloughter <tristan.sloughter>
%%% @doc
%%% @end
%%% @copyright 2011 Tristan Sloughter
%%%----------------------------------------------------------------
-module(epubnub_chat_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, connect/1, connect/2, disconnect/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | any().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

connect(Channel) ->
    supervisor:start_child(?SERVER, [Channel]).

connect(EPN, Channel) ->
    supervisor:start_child(?SERVER, [EPN, Channel]).

disconnect(PID) ->
    epubnub_chat:stop(PID).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================


%% @private
-spec init(list()) -> {ok, {SupFlags::any(), [ChildSpec::any()]}} |
                       ignore | {error, Reason::any()}.
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = temporary,
    Shutdown = 2000,
    Type = worker,

    AChild = {channel, {epubnub_chat, start_link, []},
              Restart, Shutdown, Type, [epubnub_chat]},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


