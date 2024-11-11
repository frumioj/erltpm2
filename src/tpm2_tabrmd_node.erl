-module(tpm2_tabrmd_node).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% DBUS interface constants
-define(TABRMD_INTERFACE, "com.intel.tss2.Tabrmd").
-define(TABRMD_PATH, "/com/intel/tss2/Tabrmd").

-record(state, {
    connections = #{},  % Map of Connection PIDs to their state
    next_connection_id = 1
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    % Initialize DBUS service
    {ok, Bus} = dbus_bus_reg:connect(system),
    ok = dbus_bus_reg:register_name(Bus, ?TABRMD_INTERFACE),
    {ok, #state{}}.

handle_call({create_connection}, _From, State = #state{next_connection_id = Id}) ->
    % Create new connection context
    {ok, ConnPid} = tpm2_connection:start_link(Id),
    NewState = State#state{
        connections = maps:put(ConnPid, Id, State#state.connections),
        next_connection_id = Id + 1
    },
    {reply, {ok, Id}, NewState};

handle_call({transmit, ConnId, Command}, _From, State) ->
    case find_connection(ConnId, State) of
        {ok, ConnPid} ->
            Reply = tpm2_connection:transmit(ConnPid, Command),
            {reply, Reply, State};
        error ->
            {reply, {error, invalid_connection}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Helper function to find a connection by ID
find_connection(ConnId, #state{connections = Connections}) ->
    case lists:keyfind(ConnId, 2, maps:to_list(Connections)) of
        {ConnPid, _} -> {ok, ConnPid};
        false -> error
    end.
    