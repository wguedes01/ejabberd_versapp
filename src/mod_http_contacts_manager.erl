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

-define(DB_IP, <<"gcloudsql.dev.versapp.co">>).

-behaviour(gen_mod).

-export([
    start/2,
    stop/1,
    process/2
    ]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").
-include("logger.hrl").

%%%----------------------------------------------------------------------
%%% REQUEST HANDLERS
%%%----------------------------------------------------------------------

process([<<"store">>], Request) ->

	%%Extract parameters we want from Request:
	{request,'POST',_PATH,_Q , _Something, _Undef  , _Lang  , Data , _A, _Host, _P , _T , _I} = Request,

	ContactList = string:tokens(binary_to_list(Data), ","),
	
	RegisteredList = lists:any(fun(ContactId)-> 

		?INFO_MSG("Contact Id: ~p", [ContactId]),

		{_,_, Reg} = ejabberd_odbc:sql_query(<<"ce.dev.versapp.co">>,
                                [<<"SELECT username FROM username_phone_email WHERE CONCAT(ccode,phone)='">>,ContactId,<<"' OR email='">>,ContactId,<<"'">>]),	

		?INFO_MSG("Done w query", []),

		length(Reg) > 0

		end, ContactList),	
		


	?INFO_MSG("Request: ~p", [RegisteredList]);
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
