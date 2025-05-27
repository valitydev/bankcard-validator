%%%
%%% Copyright 2021 RBKmoney
%%% Copyright 2022 Vality.dev
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

-module(bankcard_validator_ct_helper).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_conf_v2_thrift.hrl").

%% API
-export([init_suite/2]).
-export([stop_mocked_service_sup/1]).

-define(SERVICE_IP, "::").
-define(SERVICE_HOST_NAME, "localhost").
-define(PAYMENT_SYSTEM_REF(ID), {payment_system, #domain_PaymentSystemRef{id = ID}}).
-define(PAYMENT_SYSTEM_OBJ(ID, Rules),
    {payment_system, #domain_PaymentSystemObject{
        ref = #domain_PaymentSystemRef{id = ID},
        data = #domain_PaymentSystem{
            name = ID,
            validation_rules = Rules
        }
    }}
).

-type config() :: [{atom(), any()}].
-type app_name() :: atom().

-spec init_suite(module(), config()) -> config().
init_suite(Module, Config) ->
    SupPid = start_mocked_service_sup(Module),
    Apps1 = start_app(woody, [{acceptors_pool_size, 4}]),
    AllObjects = #{
        ?PAYMENT_SYSTEM_REF(<<"VISA">>) =>
            ?PAYMENT_SYSTEM_OBJ(
                <<"VISA">>,
                bankcard_validator_legacy:get_payment_system_ruleset(<<"VISA">>)
            ),
        ?PAYMENT_SYSTEM_REF(<<"DUMMY">>) =>
            ?PAYMENT_SYSTEM_OBJ(<<"DUMMY">>, undefined)
    },
    ServiceURLs = mock_services_(
        [
            {
                'RepositoryClient',
                {dmsl_domain_conf_v2_thrift, 'RepositoryClient'},
                fun('CheckoutObject', {{version, 1}, ObjectRef}) ->
                    case maps:get(ObjectRef, AllObjects, undefined) of
                        undefined ->
                            woody_error:raise(business, #domain_conf_v2_ObjectNotFound{});
                        Object ->
                            {ok, #domain_conf_v2_VersionedObject{
                                info = #domain_conf_v2_VersionedObjectInfo{
                                    version = 1,
                                    changed_at = genlib_rfc3339:format(genlib_time:unow(), second),
                                    changed_by = #domain_conf_v2_Author{
                                        id = ~b"TEST",
                                        name = ~b"TEST",
                                        email = ~b"TEST"
                                    }
                                },
                                object = Object
                            }}
                    end
                end
            },
            {
                'Repository',
                {dmsl_domain_conf_v2_thrift, 'Repository'},
                fun('GetLatestVersion', _) ->
                    {ok, 1}
                end
            }
        ],
        SupPid
    ),
    Apps2 =
        start_app(dmt_client, [{max_cache_size, #{}}, {service_urls, ServiceURLs}, {cache_update_interval, 50000}]),
    [{apps, lists:reverse(Apps1 ++ Apps2)}, {suite_test_sup, SupPid} | Config].

-spec start_mocked_service_sup(module()) -> pid().
start_mocked_service_sup(Module) ->
    {ok, SupPid} = supervisor:start_link(Module, []),
    _ = unlink(SupPid),
    SupPid.

-spec start_app(app_name(), list()) -> [app_name()].
start_app(AppName, Env) ->
    genlib_app:start_application_with(AppName, Env).

-spec mock_services_(_, _) -> _.
mock_services_(Services, SupPid) ->
    Name = lists:map(fun get_service_name/1, Services),
    {ok, IP} = inet:parse_address(?SERVICE_IP),
    ServerID = {dummy, Name},
    WoodyOpts = #{
        ip => IP,
        port => 0,
        event_handler => woody_event_handler_default,
        handlers => lists:map(fun mock_service_handler/1, Services)
    },
    ChildSpec = woody_server:child_spec(ServerID, WoodyOpts),
    {ok, _} = supervisor:start_child(SupPid, ChildSpec),
    {_IP, Port} = woody_server:get_addr(ServerID, WoodyOpts),
    lists:foldl(
        fun(Service, Acc) ->
            ServiceName = get_service_name(Service),
            Acc#{ServiceName => make_url(ServiceName, Port)}
        end,
        #{},
        Services
    ).

get_service_name({ServiceName, _Fun}) ->
    ServiceName;
get_service_name({ServiceName, _WoodyService, _Fun}) ->
    ServiceName.

mock_service_handler({ServiceName, WoodyService, Fun}) ->
    mock_service_handler(ServiceName, WoodyService, Fun).

mock_service_handler(ServiceName, WoodyService, Fun) ->
    {make_path(ServiceName), {WoodyService, {bankcard_validator_dummy_service, #{function => Fun}}}}.

make_url(ServiceName, Port) ->
    iolist_to_binary(["http://", ?SERVICE_HOST_NAME, ":", integer_to_list(Port), make_path(ServiceName)]).

make_path(ServiceName) ->
    "/" ++ atom_to_list(ServiceName).

-spec stop_mocked_service_sup(pid()) -> _.
stop_mocked_service_sup(SupPid) ->
    proc_lib:stop(SupPid, shutdown, 5000).
