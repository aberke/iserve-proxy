-module(proxy_cb).
-export([start/1, iserve_request/3]).

% for spawning server that handles asset requests
-export([asset_server/1]).

-include_lib("../../../include/iserve.hrl").

-behaviour(iserve).

%%------------------------------------------------------------------------
% Proxy Server:
% 	When /proxy/target-site requested, render target-site assets as if it is our site
% When a request is served we get the (presumably) HTML file.  
% Problem:
% 	HTML file will include assets that we mustmake subsequent requests to fetch.  
% 	However, when the assets are referred to by a relative path 
% 		- must resolve absolute path and fetch the assets.
% Solution:
% 	asset_server maintains state machine.
% 		- For each proxy requested, remember UrlBase so that subsequent requests can 
% 		  concatenate UrlBase + RelativePath
%%------------------------------------------------------------------------

start(Port) ->
    iserve:add_server(iserve_master, Port, ?MODULE, none).

iserve_request(Path, _C, Req) ->
    error_logger:info_report(
      lists:zip(record_info(fields, req), tl(tuple_to_list(Req)))),

    Headers = [{'Content-Type', "text/html"}],
    iserve:reply_ok(Headers, body(binary_to_list(Path))).


% helper method to body.  Fetch the asset requested from the target site.
get_asset(UrlBase, RelativePath) ->
	get_asset(string:concat(UrlBase, RelativePath)).
get_asset(Path) ->
	inets:start(),
	AbsPath = string:concat("http://", Path),
	{ok, {_Status, _Headers, Body}} = httpc:request(AbsPath),
	inets:stop(),
	Body.

% state machine server to remember UrlBase to resolve absolute paths of assets
% called by body() to fetch UrlBase
asset_server(UrlBase) ->
	receive
		finished ->
			exit(normal);
		{Pid, urlbase} -> 
			Pid ! UrlBase,
			asset_server(UrlBase)
	end.

% There is a brand new proxy target, so start up the asset_server with the UrlBase.
% stop the asset server if it is currently running.
-spec start_asset_server(UrlBase :: string()) -> ok.
start_asset_server(UrlBase) ->
	case whereis(asset_server_proccess) of
		undefined -> 
			register(asset_server_proccess, spawn(?MODULE, asset_server, [UrlBase]));
		_ ->
			asset_server_proccess ! finished,
			start_asset_server(UrlBase)
	end.


% Helper method called by iserve_request.
% Returns the Body of the HTTP Response
-spec body(Path :: string()) -> binary().
body("/") ->
	% index page that takes input as proxy target-sites
	welcome();
body([$/,$p,$r,$o,$x,$y,$/ | Rest]) ->
	% If GET request matches /proxy/[REST] --> serve up new proxy
	start_asset_server(Rest),
	get_asset(Rest);
body(Path) ->
	% This should be a request for an asset for the current target-site or it's a bad request
	case whereis(asset_server_proccess) of
		undefined ->
			bad_use();
		_ ->
			asset_server_proccess ! {self(), urlbase},
			receive
				Path ->
					ok;
				UrlBase ->
					get_asset(UrlBase, Path)
			after 2000 ->
				yell(timeout)
			end
	end.


bad_use() ->
	% Something went wrong - return a page redirecting to index
    <<"<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
	<html>
	<head>
	  <title>Bad Use | iserve proxy</title>
	</head>
	<body>
	  <h1>Something went wrong</h1>
	  <a href='/'>Go make a proxy request</a>
	</body>
	</html>
	">>.

% serves the index page.  With style and pizzazz.
welcome() ->
    <<"<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html>
<head>
  <title>Proxy | iserve</title>
</head>
<body>
	<style>
	#container {
		max-width: 600px;
		margin: 50px auto;
		padding: 20px;
		text-align: center;
		border: 3px solid gainsboro;
		color: darkcyan;
		font-family: arial;
		background-color: aliceblue;
	}
	input {
		padding: 1px 3px;
	}
	button {
		background-color: white;
		border: 2px solid gray;
		color: gray;
		font-weight: bold;
		border-radius: 6px;
		cursor: pointer;
	}
	button:hover {
		background-color: gainsboro;
	}
	ul {
		list-style-type: none;
		padding: 0;
	}
	</style>
	<script>
		function submit() {
			var value = document.getElementById('input').value;
			console.log(value);
			window.location.href = ('proxy/' + value);
			console.log(window.location.href);
		}
	</script>
	<div id='container'>
		<h1>Welcome</h1>
		<p>Tell me what page you want to see and I'll keep it a secret that you ever asked.</p>
		<label>http://</label>
		<input id='input' placeholder='www.google.com'>
		<button onclick='submit()'>PROXY THIS</button>
		<p>Examples:</p>
		<ul>
		<li><a href='/proxy/www.google.com'>www.google.com</a>
		<li><a href='/proxy/www.wikipedia.com'>www.wikipedia.com</a>
		<li><a href='/proxy/www.stackoverflow.com'>www.stackoverflow.com</a>
		<li><a href='/proxy/www.erlang.com'>www.erlang.com</a>
	</ul>
	</div>
</body>
</html>
">>.










% for my loud debug calls
yell() ->
	io:format("****************************~nYELLING~n**************************~n").
yell(Param) ->
	io:format("****************************~nYELLING~n~w~n**************************~n", [Param]).