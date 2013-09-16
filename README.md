erl-metafeed
============

erl-metafeed is mashup engine written in Erlang for aggregation,
remixing and filtering of web feeds.

Installation
------------

Dependencies: erlang and yaws.

running erl-metafeed web server on http://localhost:8001:

        make www

or as deamon:

   yaws -D --pa ebin --runmod mf

playing with internals:

        make

running tests:

        make test
