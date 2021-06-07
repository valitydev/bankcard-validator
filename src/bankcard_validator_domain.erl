%%%
%%% Copyright 2021 RBKmoney
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%

-module(bankcard_validator_domain).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").

-export([get_payment_system_ruleset/2]).

-type context() :: woody_context:ctx().
-type payment_system_id() :: binary().
-type validation_rule() :: dmsl_domain_thrift:'PaymentCardValidationRule'().
-type validation_rules() :: ordsets:ordset(validation_rule()) | undefined.

-export_type([validation_rules/0]).
-export_type([payment_system_id/0]).

-spec get_payment_system_ruleset(payment_system_id(), context()) -> {ok, validation_rules()} | {error, not_found}.
get_payment_system_ruleset(ID, Context) ->
    #'Snapshot'{domain = Domain} = get_shapshot(Context),
    case dmt_domain:get_object({payment_system, #domain_PaymentSystemRef{id = ID}}, Domain) of
        {ok, {payment_system, #domain_PaymentSystemObject{data = #domain_PaymentSystem{validation_rules = Ruleset}}}} ->
            {ok, Ruleset};
        error ->
            {error, not_found}
    end.

get_shapshot(Context) ->
    get_shapshot(head(), Context).

get_shapshot(Reference, _Context) ->
    dmt_client:checkout(Reference).

head() ->
    {'head', #'Head'{}}.
