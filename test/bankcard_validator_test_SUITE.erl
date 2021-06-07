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

-module(bankcard_validator_test_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

-export([init/1]).

-export([test_no_rules_found/1]).
-export([test_invalid_card_number_checksum/1]).
-export([test_invalid_card_number_range/1]).
-export([test_invalid_cvc/1]).
-export([test_invalid_exp_date/1]).
-export([test_empty_ruleset/1]).
-export([test_empty_legacy_ruleset/1]).

-type config() :: [{atom(), any()}].
-type case_name() :: atom().

-spec all() -> [case_name()].
-spec init_per_suite(config()) -> config().
-spec end_per_suite(config()) -> any().
-spec test_no_rules_found(config()) -> ok.
-spec test_invalid_card_number_checksum(config()) -> ok.
-spec test_invalid_card_number_range(config()) -> ok.
-spec test_invalid_cvc(config()) -> ok.
-spec test_invalid_exp_date(config()) -> ok.
-spec test_empty_ruleset(config()) -> ok.
-spec test_empty_legacy_ruleset(config()) -> ok.

-behaviour(supervisor).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

%%
%% tests descriptions
%%
all() ->
    [
        test_no_rules_found,
        test_invalid_card_number_checksum,
        test_invalid_cvc,
        test_invalid_exp_date,
        test_invalid_card_number_range,
        test_empty_ruleset,
        test_empty_legacy_ruleset
    ].

%%
%% starting/stopping
%%
init_per_suite(C) ->
    bankcard_validator_ct_helper:init_suite(?MODULE, C).

end_per_suite(C) ->
    _ = bankcard_validator_ct_helper:stop_mocked_service_sup(?config(suite_test_sup, C)),
    _ = [application:stop(App) || App <- proplists:get_value(apps, C)],
    ok.

%%
%% tests
%%
test_no_rules_found(_C) ->
    DefaultEnv = #{now => calendar:universal_time()},
    try
        ok = bankcard_validator:validate(
            #{card_number => <<"12345678909887">>, exp_date => {2, 2020}},
            <<"NONEXISTED">>,
            DefaultEnv,
            #{deadline => undefined, rpc_id => #{}}
        ),
        error(not_reachable)
    catch
        throw:{invalid, payment_system} -> ok
    end.

test_invalid_card_number_checksum(_C) ->
    DefaultEnv = #{now => calendar:universal_time()},
    {error, {invalid, card_number, luhn}} =
        bankcard_validator:validate(
            #{card_number => <<"12345678909887">>, exp_date => {2, 2030}},
            <<"VISA">>,
            DefaultEnv,
            #{deadline => undefined, rpc_id => #{}}
        ),
    ok.

test_invalid_card_number_range(_C) ->
    DefaultEnv = #{now => calendar:universal_time()},
    {error, {invalid, card_number, {ranges, _}}} =
        bankcard_validator:validate(
            #{card_number => <<"424242424242424242">>, exp_date => {2, 2030}, cvc => <<"12345">>},
            <<"VISA">>,
            DefaultEnv,
            #{deadline => undefined, rpc_id => #{}}
        ),
    ok.

test_invalid_cvc(_C) ->
    DefaultEnv = #{now => calendar:universal_time()},
    {error, {invalid, cvc, {length, _, _}}} =
        bankcard_validator:validate(
            #{card_number => <<"4242424242424242">>, exp_date => {2, 2030}, cvc => <<"12345">>},
            <<"VISA">>,
            DefaultEnv,
            #{deadline => undefined, rpc_id => #{}}
        ),
    ok.

test_invalid_exp_date(_C) ->
    DefaultEnv = #{now => calendar:universal_time()},
    {error, {invalid, exp_date, expiration}} =
        bankcard_validator:validate(
            #{card_number => <<"4242424242424242">>, exp_date => {2, 2020}},
            <<"VISA">>,
            DefaultEnv,
            #{deadline => undefined, rpc_id => #{}}
        ),
    ok.

test_empty_ruleset(_C) ->
    DefaultEnv = #{now => calendar:universal_time()},
    ok = bankcard_validator:validate(
        #{card_number => <<"42424242424242424242">>, exp_date => {2, 2020}},
        <<"DUMMY">>,
        DefaultEnv,
        #{deadline => undefined, rpc_id => #{}}
    ),
    ok.

test_empty_legacy_ruleset(_C) ->
    DefaultEnv = #{now => calendar:universal_time()},
    ok = bankcard_validator:validate(
        #{card_number => <<"42424242424242424242">>, exp_date => {2, 2020}},
        <<"UZCARD">>,
        DefaultEnv,
        #{deadline => undefined, rpc_id => #{}}
    ),
    ok.
