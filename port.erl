-module(port).
-export([f/2, zips/1, zips/2, send/1, send/2, start/0, start/1]).
-import(c_util, [zip/2, prod/1]).
%-import(h_html, [with_newline/1]).

f(U,P) -> {U,P}.

with_newline(S) -> lists:reverse([10 | lists:reverse(S)]).

start()   -> open_port({spawn, "python port.py"}, [stream]) .
start(1)  -> [ port:start() ]; 
start(N)  -> [ port:start() | start(N-1) ].


send(Port, Msg) ->
    port_command(Port, Msg),
    read_port(Port).

send(Msg) ->
    Port = start(),
    {Atom, Response} = send(Port, Msg).

read_port(Port) ->
    receive {Port, {data, Result}} ->
        {ok, Result}
    after
        5000 ->
            {error, timeout}
    end.


zips(Urls) ->
    [ send(U) || U <- Urls ].

zips(Urls, Ports) ->
    [ send(P, U) || [U,P] <- zip(Urls, Ports)].



% c(port).
% Two different ways to send a message/response to Python.
% 1 ok
% P = port:start().
% port:send(P,  "yikes\n").
% 
% 2 no
% spawn(port, send, ["monkey\n"])   Fails
% 
% 3 ok
% [port:send(X) || X <- ["acb\n", "xyz\n"]]. 
% Creates a new python process for each message.
% 
% U  = ["http://www.erlang.org" , "http://www.python.org" , "http://www.google.com"].
% U  = ["http://www.erlang.org\n" , "http://www.python.org\n" , "http://www.google.com\n"].
% PM = port:start(3).
% MS = ["acb\n", "xyz\n", "pdq\n"]. 
% port:zips(MS, PM).
% port:zips(MS).
% port:send("foo\n").
% [A,B,C] = PM.
% port:send(A, "foo\n").


