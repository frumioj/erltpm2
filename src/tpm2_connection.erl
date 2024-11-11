-module(tpm2_connection).
-behaviour(gen_server).

-export([start_link/1, transmit/2, receive_response/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    id,
    tpm_context,
    last_command
}).

start_link(Id) ->
    gen_server:start_link(?MODULE, [Id], []).

transmit(Pid, Command) ->
    gen_server:call(Pid, {transmit, Command}).

receive_response(Pid) ->
    gen_server:call(Pid, receive_response).

init([Id]) ->
    % Initialize TPM context using NIF
    {ok, Context} = tpm2_nif:init_context(),
    {ok, #state{id = Id, tpm_context = Context}}.

handle_call({transmit, Command}, _From, State) ->
    case tpm2_nif:transmit(State#state.tpm_context, Command) of
        {ok, Response} ->
            {reply, {ok, Response}, State#state{last_command = Command}};
        Error ->
            {reply, Error, State}
    end;

handle_call(receive_response, _From, State) ->
    % Implement logic to receive response
    {reply, {ok, "response"}, State};

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