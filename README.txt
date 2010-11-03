Author  : klemo <klemo.vladimir@gmail.com>
Created :  8 Sep 2010 by klemo <klemo.vladimir@gmail.com>

Erl-metafeed is mashup engine written in Erlang for aggregation,
remixing and filtering of web feeds.

Source code: http://gitorious.org/erl-metafeed

=== Installation instructions ===

Dependencies: erlang and yaws.

running erl-metafeed web server on http://localhost:8001:
> make www

or as deamon:
> yaws -D --pa ebin --runmod mf

playing with internals:
> make

running tests:
> make test
