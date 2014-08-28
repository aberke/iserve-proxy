-module(iserve_app).
-behaviour(application).
-export([
	 start/2,
	 stop/1
        ]).

start(_Type, _StartArgs) ->
    io:format("iserve_app.erl start(~w, ~w)*************~n", [_Type, _StartArgs]),
    case iserve_sup:start_link() of
	{ok, Pid} -> 
	    {ok, Pid};
	Error ->
	    Error
    end.

stop(_State) ->
    ok.
