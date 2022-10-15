%%%-------------------------------------------------------------------
%%% @author evoowil
%%% @copyright (C) 2022, evoowil
%%% @doc
%%%
%%% @end
%%% Created : 2022-10-07 20:06:52.210164
%%%-------------------------------------------------------------------
-module(game).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         broadcast/2,
         update_player/2,
         create_player/2,
         code_change/3]).
-on_load(start/0).

-define(SERVER, ?MODULE).
-define(UPDATE_STATE_TIMER, 10000).
-define(BROADCAST_TIMER, 10000).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    spawn_link(?MODULE, broadcast, [self(),?BROADCAST_TIMER]),
    spawn_link(?MODULE, update_player, [self(),?UPDATE_STATE_TIMER]),
    gen_server:cast(self(), start_player),
    {ok, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
%% Indentifier is based on the PID!
handle_cast(start_player , State) ->
    player:start_link(self()),
    {noreply, State};

handle_cast({make_player, Player} , State) ->
    io:format("making player...."),
    {noreply, [{map_get("pid",Player),Player}] ++ State};

handle_cast(send_broadcast, State) ->
    io:format("Player state is:  ~p~n",[State]),
    {noreply, State};
handle_cast(update_player_state, State) ->
    NewState = update_coords(State),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    io:format("game module got message"),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({"start_player"} , State) ->
    io:format("Player state is:  ~p~n",[State]),
    player:start_link(self()),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        io:format("The process changed"),
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%%

broadcast(Pid,BroadCastDelay) ->
    gen_server:cast(Pid, send_broadcast),
    timer:sleep(BroadCastDelay),
    broadcast(Pid,BroadCastDelay).

update_player(Pid,BroadCastDelay) ->
    gen_server:cast(Pid, update_player_state),
    timer:sleep(BroadCastDelay),
    update_player(Pid,BroadCastDelay).
create_player(Pid,BroadCastDelay) ->
    gen_server:cast(Pid, create_player),
    timer:sleep(BroadCastDelay),
    create_player(Pid,BroadCastDelay).

start() ->
    io:format("loading..."),
    init([]),
    ok.

update_coords(Entities) ->

[ {Pid,Player#{"pos" := {X+DX,Y+DY}}} || {Pid,Player = #{"pos" := {X,Y},"direction" := {DX,DY}} }<- Entities].

% player_to_jsonB(Player) ->
%     Player = #{"pos" := {X,Y},"direction" := {DX,DY}, "pid" := Pid}
%     <<"{pos :" list_to_binary()  \n">>


