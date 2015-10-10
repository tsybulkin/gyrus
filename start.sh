#! /bin/bash
./rebar co && erl -pa ebin deps/*/ebin -s gyrus_app
