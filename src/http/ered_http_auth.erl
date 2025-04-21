-module(ered_http_auth).
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
    case
        {
            is_protected(Req),
            os:getenv("ERED_USER"),
            os:getenv("ERED_PASS"),
            cowboy_req:parse_header(<<"authorization">>, Req)
        }
    of
        % We cam skeddaddle if it's not a protected path
        {false, _, _, _} ->
            {ok, Req, Env};
        % Both enviornment values should be set to enable Basic Auth
        {_, false, _, _} ->
            {ok, Req, Env};
        {_, _, false, _} ->
            {ok, Req, Env};
        % They are set but the client didn't send auth request
        {true, _, _, undefined} ->
            say_no(Req);
        {true, User, Pass, {basic, Buser, Bpass}} ->
            case {string:equal(User, Buser), string:equal(Pass, Bpass)} of
                % And, of course, we want them to match
                {true, true} -> {ok, Req, Env};
                % The credentials didn't match, be insistent
                _ -> say_no(Req)
            end
    end.

%% Convenience function to send the auth demand
say_no(Req) ->
    {stop,
        cowboy_req:reply(401, #{<<"www-authenticate">> => <<"Basic realm=\"Erlang-RED\"">>}, Req)}.

% Using the request object because we might want additional different logic later
is_protected(#{path := Path}) ->
    case Path of
        %% A couple exact matches
        <<"/flows.initial.json">> -> true;
        <<"/settings.json">> -> true;
        %% This covers a couple directories, actually
        <<"/node", _/binary>> -> true;
        %% We can reuse for the wrapper site
        <<"/red/images/", _/binary>> -> false;
        <<"/red/", _/binary>> -> true;
        %% And just to prevent spidering around when its
        %% obvious what paths might exist
        <<"/plugins/", _/binary>> -> true;
        <<"/vendor/", _/binary>> -> true;
        <<"/settings/", _/binary>> -> true;
        <<"/types/", _/binary>> -> true;
        _ -> false
    end.
