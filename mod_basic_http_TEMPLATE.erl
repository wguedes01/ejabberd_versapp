%%%----------------------------------------------------------------------
%%% File    : 
%%% Author  : Will Brazil
%%% Purpose : Sample module that extends embedded ejabberd HTTP server
%%% Created :
%%% Id      :
%%%----------------------------------------------------------------------

-module(mod_http_contacts_manager).
-author('willbrazil.usa@gmail.com').
-vsn('').
-define(ejabberd_debug, true).

-behaviour(gen_mod).

-export([
    start/2,
    stop/1,
    process/2
    ]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").

%%%----------------------------------------------------------------------
%%% REQUEST HANDLERS
%%%----------------------------------------------------------------------

process([<<"store">>], _Request) ->
	"Hey Steve";
process(["produce_error"], _Request) ->
    {400, [], {xmlelement, "h1", [],
               [{xmlcdata, "400 Bad Request"}]}};

process(LocalPath, _Request) ->
    {xmlelement, "html", [{"xmlns", "http://www.w3.org/1999/xhtml"}],
     [{xmlelement, "head", [],
       [{xmlelement, "title", [], []}]},
      {xmlelement, "body", [],
       [{xmlelement, "p", [], [{xmlcdata, io_lib:format("Called with path: ~p", [LocalPath])}]}]}]}.

%%%----------------------------------------------------------------------
%%% BEHAVIOUR CALLBACKS
%%%----------------------------------------------------------------------

start(_Host, _Opts) ->
    ok.

stop(_Host) ->
    ok. 
