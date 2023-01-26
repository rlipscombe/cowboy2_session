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
                             stream_handlers => [cowboy2_session_stream_h, cowboy_stream_h]}),
```

It adds a `session` key to the `Req` object in all of your handlers:

```erlang
init(Req = #{session := Session}, Opts) ->
    %...
    {ok, Req#{session => NewSession}, Opts}.
```

## What's missing?

It's brand-new, so there's some bits missing.

- You can't configure the cookie options, so no `HttpOnly`, `Secure`, `SameSite`, etc.
- You can't configure the cookie name.
- ...or the ETS table name.
- ...or the default session object.
- Sessions don't expire and can't be revoked.
- The only storage supported is ETS, so you can't use it behind a load-balancer.
