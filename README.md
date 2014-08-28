iserve-proxy
===

Proxy server written in Erlang.  
Check it out: <http://iserve-proxy.herokuapp.com/>

Builds with rebar.  Example use of iserve.

Originally forked iserve from <https://github.com/noss/iserve>


Run
---

- Download and compile rebar
```
$ wget https://raw.github.com/wiki/rebar/rebar/rebar && chmod u+x rebar
$ mv rebar <some-dir-on-PATH>
```
- Clone or fork this repository and ```$ cd iserve-proxy```
- Compile code ```$ rebar compile```
- Run server ```$ erl -pa ebin modules/proxy/ebin -s iserve```
- Visit <http://localhost:6464/>

Stop the server:
```> CNTL+G q```

Clean: ```$ rebar clean```


Learn
---

- Walk through of code: <https://erlangcentral.org/wiki/index.php?title=A_fast_web_server_demonstrating_some_undocumented_Erlang_features>
- Inspect ```proxy_app.erl``` and ```proxy_cb.erl``` for the source code for this app, and ```proxy.app``` for the configuration of it.