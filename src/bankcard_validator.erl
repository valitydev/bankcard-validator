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

-module(bankcard_validator).

-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([validate/4]).

-type payment_system() :: binary().
-type context() :: woody_context:ctx().
-type bankcard_data() :: #{
    card_number := binary(),
    cardholder => binary() | undefined,
    exp_date => {integer(), integer()},
    cvc => binary()
}.

-type reason() ::
    {invalid, card_number, luhn}
    | {invalid, card_number, {ranges, [{range, pos_integer(), pos_integer()}]}}
    | {invalid, cvc, {length, pos_integer(), pos_integer()}}
    | {invalid, exp_date, expiration}.

-type validation_env() :: #{
    now := calendar:datetime()
}.

-export_type([bankcard_data/0]).
-export_type([payment_system/0]).
-export_type([validation_env/0]).
-export_type([reason/0]).

-spec validate(bankcard_data(), payment_system(), validation_env(), context()) -> ok | {error, reason()}.
validate(CardData, PaymentSystem, Env, Context) ->
    Ruleset = get_ruleset(PaymentSystem, Context),
    try
        run_assertions(CardData, Ruleset, Env)
    catch
        _:Reason ->
            {error, Reason}
    end.

run_assertions(_CardData, undefined, _Env) ->
    %% This means there is no known validation assertions
    %% for some kind of payment system, for example, DUMMY
    ok;
run_assertions(CardData, Assertions, Env) ->
    lists:foreach(
        fun
            ({K, Checks}) when is_list(Checks) ->
                V = maps:get(K, CardData, undefined),
                lists:foreach(
                    fun(C) -> check_value(V, C, Env) orelse erlang:throw({invalid, K, convert(C)}) end,
                    Checks
                );
            ({K, Check}) ->
                V = maps:get(K, CardData, undefined),
                check_value(V, Check, Env) orelse erlang:throw({invalid, K, convert(Check)})
        end,
        Assertions
    ).

check_value(undefined, _, _) ->
    true;
check_value(V, {ranges, Ranges}, _) ->
    lists:any(fun(L) -> check_range(V, L) end, Ranges);
check_value(V, {length, Length}, _) ->
    lists:any(fun(L) -> check_range(V, L) end, [Length]);
check_value(V, {checksum, {luhn, #domain_PaymentCardNumberChecksumLuhn{}}}, _) ->
    check_luhn(V, 0);
check_value({M, Y}, {exact_exp_date, #domain_PaymentCardExactExpirationDate{}}, #{
    now := {{Y0, M0, _DD}, _Time}
}) ->
    M >= 1 andalso
        M =< 12 andalso
        {Y, M} >= {Y0, M0}.

check_range(V, #'IntegerRange'{lower = L, upper = U}) ->
    L =< byte_size(V) andalso byte_size(V) =< U.

check_luhn(<<CheckSum>>, Sum) ->
    case Sum * 9 rem 10 of
        M when M =:= CheckSum - $0 ->
            true;
        _M ->
            false
    end;
check_luhn(<<N, Rest/binary>>, Sum) when byte_size(Rest) rem 2 =:= 1 ->
    case (N - $0) * 2 of
        M when M >= 10 ->
            check_luhn(Rest, Sum + M div 10 + M rem 10);
        M ->
            check_luhn(Rest, Sum + M)
    end;
check_luhn(<<N, Rest/binary>>, Sum) ->
    check_luhn(Rest, Sum + N - $0).

get_ruleset(PaymentSystem, Context) ->
    case bankcard_validator_domain:get_payment_system_ruleset(PaymentSystem, Context) of
        {error, not_found} ->
            %% If no payment system with given id found in domain config
            %% try to use legacy ruleset (if any)
            %% TODO Remove after migration to dominant
            bankcard_validator_legacy:get_payment_system_ruleset(PaymentSystem);
        {ok, Ruleset} ->
            Ruleset
    end.

convert({checksum, {luhn, _}}) -> luhn;
convert({ranges, Ranges}) -> {ranges, [{range, L, U} || #'IntegerRange'{upper = U, lower = L} <- Ranges]};
convert({length, #'IntegerRange'{upper = U, lower = L}}) -> {length, L, U};
convert({exact_exp_date, _}) -> expiration.
