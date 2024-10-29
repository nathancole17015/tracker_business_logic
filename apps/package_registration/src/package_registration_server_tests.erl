-module(package_registration_server_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    {ok, Pid} = package_registration_server:start_link(),
    {ok, Pid}.

teardown(Pid) ->
    package_registration_server:stop(),
    ok.

% Test registration
register_package_test() ->
    Pid = setup(),
    %% Check if registration works
    ?assertMatch({ok, _}, package_registration_server:register_package(1, <<"Sender1">>, <<"Receiver1">>)),
    teardown(Pid).

% Additional tests can be added here
