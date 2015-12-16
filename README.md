riak_pb_msgcodegen
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { riak_pb_msgcodegen, ".*", {git, "git@host:user/riak_pb_msgcodegen.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 riak_pb_msgcodegen
    ===> Fetching riak_pb_msgcodegen
    ===> Compiling riak_pb_msgcodegen
    <Plugin Output>
