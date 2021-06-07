-module(prop_bankcard_validation_valid_carddata).
-include_lib("proper/include/proper.hrl").
-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("common_test/include/ct.hrl").

-export([prop_test/0]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
-spec prop_test() -> any().
%% Suppress proper's internal type mismatch for setup generator
-dialyzer([no_return, no_opaque]).
prop_test() ->
    ?SETUP(
        fun() ->
            meck:new([bankcard_validator_domain], [passthrough]),
            meck:expect(bankcard_validator_domain, get_payment_system_ruleset, fun(ID, _Context) ->
                {ok, bankcard_validator_legacy:get_payment_system_ruleset(ID)}
            end),
            fun() ->
                meck:unload(bankcard_validator_domain)
            end
        end,
        ?FORALL(
            {PaymentSystem, CardData},
            payment_system_and_card_data(),
            check_valid_card_data(PaymentSystem, CardData)
        )
    ).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
check_valid_card_data(PaymentSystem, Card) ->
    DefaultEnv = #{now => calendar:universal_time()},
    ok =:= bankcard_validator:validate(Card, PaymentSystem, DefaultEnv, #{deadline => undefined, rpc_id => #{}}).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
%% Retrieves name of one of known rules
known_payment_system() ->
    oneof(bankcard_validator_legacy:get_known_rule_names()).

payment_system_and_card_data() ->
    ?LET(
        PaymentSystem,
        known_payment_system(),
        ?LET(
            CardData,
            valid_card_data(PaymentSystem),
            {PaymentSystem, CardData}
        )
    ).

%% Generates valid card data according payment system rules
valid_card_data(PaymentSystem) ->
    ?LET(
        {Cardholder, CVC, InvalidExpDate, CardNumberLength},
        {cardholder(), get_cvc(PaymentSystem), valid_exp_date(), get_card_number_length(PaymentSystem)},
        ?LET(
            CardNumber,
            vector(CardNumberLength, choose($0, $9)),
            #{
                card_number => add_luhn_checksum(list_to_binary(CardNumber)),
                exp_date => InvalidExpDate,
                cvc => list_to_binary(CVC),
                cardholder => list_to_binary(Cardholder)
            }
        )
    ).

%% Generate random caldholder name
cardholder() ->
    list(oneof([choose($A, $Z), 32, $.])).

%% Generate strictly valid bank card expiration date
valid_exp_date() ->
    {{Y, M, _}, _} = calendar:universal_time(),
    Year = Y + 1,
    {M, Year}.

%%%%%%%%%%%%%%%%%%
%%%  Utilites  %%%
%%%%%%%%%%%%%%%%%%
add_luhn_checksum(CardNumber) ->
    <<CardNumber/binary, (add_luhn_checksum(CardNumber, 0))>>.

add_luhn_checksum(<<>>, Sum) ->
    $0 + (Sum * 9 rem 10);
add_luhn_checksum(<<N, Rest/binary>>, Sum) when byte_size(Rest) rem 2 =:= 0 ->
    case (N - $0) * 2 of
        M when M >= 10 ->
            add_luhn_checksum(Rest, Sum + M div 10 + M rem 10);
        M ->
            add_luhn_checksum(Rest, Sum + M)
    end;
add_luhn_checksum(<<N, Rest/binary>>, Sum) ->
    add_luhn_checksum(Rest, Sum + N - $0).

get_cvc(PaymentSystem) ->
    Rules = bankcard_validator_legacy:get_payment_system_ruleset(PaymentSystem),
    case proplists:get_value(cvc, Rules) of
        {length, #'IntegerRange'{lower = L, upper = undefined}} ->
            vector(L, choose($0, $9));
        {length, #'IntegerRange'{lower = undefined, upper = U}} ->
            vector(U, choose($0, $9));
        {length, #'IntegerRange'{lower = L, upper = U}} ->
            ?LET(
                CvcLength,
                choose(L, U),
                vector(CvcLength, choose($0, $9))
            )
    end.

get_card_number_length(PaymentSystem) ->
    Rules = bankcard_validator_legacy:get_payment_system_ruleset(PaymentSystem),
    [Ranges | _] =
        lists:filtermap(
            fun
                ({card_number, {ranges, Ranges}}) -> {true, Ranges};
                (_) -> false
            end,
            Rules
        ),
    oneof(get_possible_lengths(Ranges, ordsets:new())).

get_possible_lengths([], Acc) ->
    ordsets:to_list(Acc);
get_possible_lengths([#'IntegerRange'{lower = L, upper = undefined} | Rest], Acc) ->
    get_possible_lengths(Rest, ordsets:add_element(L - 1, Acc));
get_possible_lengths([#'IntegerRange'{lower = undefined, upper = U} | Rest], Acc) ->
    get_possible_lengths(Rest, ordsets:add_element(U - 1, Acc));
get_possible_lengths([#'IntegerRange'{lower = L, upper = U} | Rest], Acc) ->
    get_possible_lengths(Rest, ordsets:add_element(choose(L - 1, U - 1), Acc)).
