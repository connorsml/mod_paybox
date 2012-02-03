%% @author Michael Connors <michael@bring42.net>
%% @copyright 2012 Michael Connors
%% @date 2012-01-19
%% @doc Webmachine-based PayBox callback listener.
%% @end

%% Copyright 2012 Michael Connors
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(resource_paybox_callback).
-author("Michael Connors <michael@bring42.net>").

-export([
         init/1,
         content_types_provided/2,
         resource_exists/2,
         allowed_methods/2,
         process_post/2,
         response/2
        ]).



-include_lib("webmachine_resource.hrl").
-include_lib("zotonic.hrl").

init([]) ->
    {ok, []}.

resource_exists(ReqData, _Context) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:ensure_qs(Context),
    {true, ReqData, Context1}.

content_types_provided(ReqData, Context) ->
    {[{"text/html", response}], ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {['POST', 'GET'], ReqData, Context}.

handle_request(Method, ReqData, Context) ->
    {Result, _Rest} = wrq:req_body(z_context:get_reqdata(Context)),
    SignedData = lists:last(lists:reverse(binary:split(Result, <<"&signature=">>))),
    OrigIP = z_context:get_req_header("X-Forwarded-For", Context),
    case lists:member(OrigIP, ["195.101.99.76", "194.2.122.158", "195.25.7.166"]) of
        true ->
            Host = Context#context.host,
            Error = z_context:get_q("error", Context), % E=The Error Code of the transaction
            Amount = z_context:get_q("amount", Context), %M=Value of Order
            OrderReference = z_context:get_q("reference", Context), % R=Order Reference
            Transaction = z_context:get_q("transaction", Context), %T=Transaction identifier
            Status = z_context:get_q("status", Context), %% O=Status - possible values: Y=success N=Failed A=Attempts processing performed
            Authorization = z_context:get_q("authorization", Context),
            Signature = z_context:get_q("signature", Context),
            DecodedSignature = list_to_binary(z_utils:url_decode(Signature)),
            Guaranteed = z_context:get_q("guaranteed", Context),
            %io:format("Error: ~s~n", [Error]),
            %io:format("Amount: ~s~n", [Amount]),
            %io:format("OrderReference: ~s~n", [OrderReference]),
            %io:format("Transaction: ~s~n", [Transaction]),
            %io:format("Status: ~s~n", [Status]),
            %io:format("Authorization: ~s~n", [Authorization]),
            %io:format("Guaranteed: ~s~n", [Guaranteed]),
      
            %%% To do: Learn more about the Authorization code from the bank

            case {Error} of
                {"00000"} -> %% Error 00000 means success/no error
                    Order = m_paybox_order:get(list_to_integer(OrderReference), Context),
                    {order_total, OrderTotal} = proplists:lookup(order_total, Order),
                    case list_to_integer(Amount) of %% Check that the amount is correct as per the order
                        OrderTotal -> %% OrderTotal is not a new variable, I am matching against the previous order total on purpose
                            KeyLocation = io_lib:format("~s", [filename:join([z_utils:lib_dir(priv), "sites", Host, "deps", "pubkey.pem"])]),
                            {ok, PemBin} = file:read_file(KeyLocation),
                            PemEntries = public_key:pem_decode(PemBin),
                            RSAPubKey = public_key:pem_entry_decode(hd(PemEntries)),

                            case public_key:verify(SignedData, sha, base64:decode(binary:list_to_bin(Signature)), RSAPubKey) of
                                true -> 
                                    Order = m_paybox_order:get(OrderReference, Context),
                                    {order_total, OrderTotal} = proplists:lookup(order_total, Order),
                                    m_paybox_order:set_paid(OrderReference, Transaction, SignedData, Context),
                                    ReqData1 = wrq:set_resp_body("", ReqData),
                                    {{halt, 200}, ReqData1, Context};
                                false ->
                                    ReqData1 = wrq:set_resp_body("NOT AUTHORIZED", ReqData), 
                                    {{halt, 403}, ReqData1, Context}  %% Signatures don't match
                            end;

                        _Other -> 
                            ReqData1 = wrq:set_resp_body("NOT AUTHORIZED", ReqData),
                            {{halt, 403}, ReqData1, Context} %% Something very wrong here, different amounts
                    end; 
                _ -> 
                    ReqData1 = wrq:set_resp_body("", ReqData),
                    {{halt, 200}, ReqData1, Context}
            end;
        false ->
            ReqData1 = wrq:set_resp_body("NOT AUTHORIZED", ReqData),
            {{halt, 403}, ReqData1, Context}
    end.

process_post(ReqData, Context) ->
    handle_request('POST', ReqData, Context).

response(ReqData, Context) ->
    handle_request('GET', ReqData, Context).