-module(cowboy2_session_test).
-include_lib("eunit/include/eunit.hrl").

-define(SESSION_ID, <<"ab_cde-fghijk_lmno-pqrst-uvwxyz0_12345-6789">>).
-define(DEFAULT_SESSION, #{}).

session_test_() ->
    {setup, fun suite_setup/0, fun suite_cleanup/1,
    [
        fun get_session_after_put_session/0
    ]}.

suite_setup() ->
    {ok, _} = application:ensure_all_started(cowboy2_session),
    ok.

suite_cleanup(_) ->
    ok.

get_session_after_put_session() ->
    Session = #{is_admin => true, wears_a_hat => true},
    % TODO: We'll need another test once we're not putting the default session in ETS (or in the Req object). See #13.
    Req0 = #{session_id => ?SESSION_ID, session => ?DEFAULT_SESSION},
    Req1 = cowboy2_session:put_session(Session, Req0),
    ?assertEqual(Session, cowboy2_session:get_session(Req1)).
