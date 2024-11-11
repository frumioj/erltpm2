-module(tpm2_tcti_tabrmd).
-export([initialize/0, transmit/2, receive_response/1]).

initialize() ->
    case net_kernel:connect_node('tpm2_tabrmd@localhost') of
        true ->
            gen_server:call({tpm2_tabrmd_node, 'tpm2_tabrmd@localhost'}, 
                           {create_connection});
        false ->
            {error, node_not_available}
    end.

transmit(ConnectionId, Command) ->
    gen_server:call({tpm2_tabrmd_node, 'tpm2_tabrmd@localhost'},
                    {transmit, ConnectionId, Command}).

receive_response(ConnectionId) ->
    gen_server:call({tpm2_tabrmd_node, 'tpm2_tabrmd@localhost'},
                    {receive_response, ConnectionId}).