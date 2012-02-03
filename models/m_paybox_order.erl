%% @author Michael Connors <michael@bring42.net>
%% @copyright 2012 Michael Connors
%% @date 2012-01-23
%%
%% @doc Model for managing orders

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

-module(m_paybox_order).
-author("Michael Connors <michael@bring42.net>").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,
    
    get/2,
    insert/7,

    set_redirect_code/3,
    set_post_order_data/4,
    set_paid/4
]).

-include_lib("zotonic.hrl").


%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()

m_find_value(get, #m{value=undefined} = M, _Context) ->
    M#m{value=get};
m_find_value(OrderId, #m{value=get}, Context) ->
    % Specific comment of the resource.
    get(OrderId, Context);
m_find_value(_Key, #m{value=undefined}, _Context) ->
   undefined.

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context)
m_to_list(_, _Context) ->
    [].

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
    undefined.


%% @spec Fetch a specific order from the database.
%% @spec get(int(), Context) -> PropList
get(OrderId, Context) ->
    z_db:assoc_props_row("select * from paybox_order where id = $1", [OrderId], Context).


%% @doc Insert a new order. Fetches the submitter information from the Context.
insert(Email, OrderDescription, OrderTotal, ExtraInfoRscId, UserId, ShippingAddress, Context) ->
    Props = [
        {user_id, UserId},
        {extra_info_rsc_id, ExtraInfoRscId},
        {shipping_address, ShippingAddress},
        {order_description, OrderDescription},
        {email, Email},
        {order_total, OrderTotal},
        {paid, false}
    ],
    case z_db:insert(paybox_order, Props, Context) of
        {ok, OrderId} = Result ->
            z_notifier:notify({paybox_order_insert, OrderId}, Context),
            Result;
        {error, _} = Error ->
            Error
    end.

%% Fix this to handle errors properly
set_paid(OrderId, TransactionId, SignedData, Context) when is_list(OrderId) ->
    set_paid(list_to_integer(OrderId), TransactionId, SignedData, Context);
set_paid(OrderId, TransactionId, SignedData, Context) ->
    z_db:update(paybox_order, OrderId, [{paid, true}, {transaction_id, TransactionId}, {signed_data, SignedData}], Context),
    z_notifier:notify({paybox_order_paid, OrderId}, Context),
    ok.

set_redirect_code(OrderId, Code, Context) ->
    z_db:update(paybox_order, OrderId, [{redirect_page, Code}], Context),
    ok.

set_post_order_data(OrderId, Code, SignedData, Context) ->
    z_db:update(paybox_order, OrderId, [{redirect_page, Code}, {signed_data, SignedData}], Context),
    ok.