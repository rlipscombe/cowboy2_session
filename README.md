# cowboy2_session

Session-handling middleware for Cowboy 2.x.

## Motivation

All of the session middleware I could find for Cowboy only seems to work with Cowboy 1.x. Here's some that works with Cowboy 2.x.

## Dependency

Update the `deps` section in your `rebar.config` as follows:

```erlang
{deps, [
  {cowboy, "2.9.0"},
  {cowboy2_session, {git, "https://github.com/rlipscombe/cowboy2_session.git"}}
]}.
```

Don't forget to add it to the `applications` list in your `.app.src` file.

## Usage

Add it to `stream_handlers` when starting cowboy, as follows:

```erlang
{ok, _} = cowboy:start_clear(http, [{port, ?PORT}],
                           #{env => #{dispatch => Dispatch},
                             % Example cookie options; change as appropriate
                             session_opts => #{cookie_opts => #{path => "/", http_only => true}},
                             % Put cowboy2_session_stream_h before cowboy_stream_h.
                             stream_handlers => [cowboy2_session_stream_h, cowboy_stream_h]}),
```

Use the session object like this:

```erlang
init(Req, Opts) ->
    % Get the session details from the request object.
    Session = cowboy2_session:get_session(Req),

    % ... do something interesting ...

    % Put updated session details in the request object.
    Req2 = cowboy2_session:put_session(NewSession, Req),
    % Send the response to the client.
    Req3 = cowboy_req:reply(200, Headers, Body, Req2),
    {ok, Req3, Opts}.
```

## What's missing?

It's brand-new, so there's some bits missing.

- You can't configure the cookie name.
- ...or the ETS table name.
- ...or the default session object.
- Sessions don't expire.
- The only storage supported is ETS, so you can't use it behind a load-balancer.
