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

-module(bankcard_validator_dummy_service).

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()} | no_return().
handle_function(FunName, Args, _, #{function := Fun}) ->
    case Fun(FunName, Args) of
        {throwing, Exception} ->
            erlang:throw(Exception);
        Result ->
            Result
    end.
