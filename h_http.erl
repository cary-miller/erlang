-module(h_http).
-export([
    async/1, sync/1, read_x/1, read_file/1,
    sync_list1/0   ,
    sync_list2/0   ,
    sync_list3/0   ,
    async_list/1   ,
    remove_last/1 ,
    with_newline/1
    ]).
-import(port2, [send/1, send/2, start/0]).

% http://erlang.org/doc/apps/inets/http_client.html
%   io:format(RespData),
%
%Url1 = "http://www.erlang.org".
%Url2 = "http://www.python.org".
test_it() ->
%    c(h_http)                        ,
    Fname = 'url_list.data'          ,
    Al = h_http:async_list(Fname)    ,
    Sl = h_http:sync_list(Fname)     .



prefix(Binary) ->
    binary_to_list(
    erlang:iolist_to_binary(["http://www.", Binary])).

remove_last(List) ->
    [Last | Rest] = lists:reverse(List),
    lists:reverse(Rest).

read_file(Filename) ->
    % Read urls from Filename and prepend http://www. to each.
    % Return a list of urls.
    % see string2lines at
    % http://stackoverflow.com/questions/2171312/the-most-efficient-way-to-read-a-file-into-a-list-of-strings
    {ok, Bin} = file:read_file(Filename),
    [ prefix(Name) || Name  <- remove_last(re:split(Bin, "\n")) ] .
    % TODO replace remove_last with remove_blank


% Request a url asynchronously.  Do not return the data.  Do return the
% RequestId.
async(Url) ->
    inets:start(),
    {ok, RequestId} = httpc:request(get, {Url, []}, [], [{sync, false}]),
    RequestId.


% Fetch the data for an asynchronous RequestId.
read_x(RequestId) ->
    receive {http, {RequestId, Result}} -> Result after 500 -> error end .


sync(Url) ->
    % synchronous.
    inets:start(),
    {ok, {{Version, Rcode, ReasonPhrase}, Headers, Body}} =
          httpc:request(get, {Url, []}, [], []),
%    {ok, {{Version, Rcode, ReasonPhrase}, Headers, Body}},
%    io:format("~p~n", [Body]),
    io:format("~p ~p ~p~n", [now(), Rcode, Url])
    .

% OK
% I've mastered fetching urls both synchronously and async.  And fetched
% back both versions via list comprehension.
% Now I want to do that in a different way.
% Send each url to an async func which sends back the result.

% time_it/1
% http://www.trapexit.org/Measuring_Function_Execution_Time
% /1
% async_list/1

sync_list_in1(Urls) ->
    T0 = now(),
    go1(Urls),
    T1 = now(),
    timer:now_diff(T1, T0)/1000000.

sync_list_in2(Urls) ->
    T0 = now(),
    go2(Urls),
    T1 = now(),
    timer:now_diff(T1, T0)/1000000.


go1(Urls) ->
    % For each Url fetch it synchronously
    [ sync(Url) || Url  <- Urls ] .

go2(Urls) ->
    % For each Url fetch it synchronously
    [ spawn(h_http, sync, [Url]) || Url  <- Urls ] .

with_newline(S) ->
    lists:reverse([10 | lists:reverse(S)]).

sync_list3() ->
    Filename = 'url_list.data'          ,
    Urls = [ with_newline(Url) || Url  <- read_file(Filename) ] ,
    [ port2:send(Url) || Url  <- Urls ] .  % good
    % sequential because not using spawn
%    [ spawn(port2, send, [Url]) || Url  <- Urls ] .  % bad



sync_list1() ->
    Filename = 'url_list.data'          ,
    Urls = read_file(Filename),
    sync_list_in1(Urls).

sync_list2() ->
    Filename = 'url_list.data'          ,
    Urls = read_file(Filename),
    sync_list_in2(Urls).

async_list_in(Urls) ->
    % For each Url fetch it asynchronously
    RequestIds = [ async(Url) || Url  <- Urls ],
    [ read_x(R) || R <- RequestIds ].


% TODO fix.  Fetches urls but gets an error.
async_list(Filename) ->
    Urls = read_file(Filename),
    async_list_in(Urls).



