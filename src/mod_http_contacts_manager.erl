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

-define(SERVER_IP, <<"ce.dev.versapp.co">>).

-behaviour(gen_mod).

-export([
    start/2,
    stop/1,
    process/2
    ]).

-export([send_packet_all_resources/3, build_packet/2]).

-export([contact_exists/3, is_valid_http_request/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").
-include("logger.hrl").

%%%----------------------------------------------------------------------
%%% REQUEST HANDLERS
%%%----------------------------------------------------------------------

process([<<"store">>], Request) ->

	%%Extract parameters we want from Request:
	{request,'POST',_PATH,_Q , _Something, _Undef  , _Lang  , Data , _A, _Host, _P , _T , Header} = Request,

	?INFO_MSG("Req: ~p", [Request]),

	%%[{Key, Username}] = lists:filter(fun({Key, Val})-> Key==<<"Username">> end, Header),

	Username = is_valid_http_request(Request),

	?INFO_MSG("\n\nUsername: ~p", [Username]),
	
	ContactList = string:tokens(binary_to_list(Data), ","),
	
	RegisteredList = lists:any(fun(ContactId)-> 

			
		%%If this user's contact is not on DB. Add it.
		case contact_exists(?SERVER_IP, Username, ContactId) of
			false ->
				%% Inserts contact into DB. I should do some checking here so it does not add duplicates.
                		ejabberd_odbc:sql_query(?SERVER_IP,
                                	[<<"INSERT INTO contacts (username, identifier) VALUES ('">>,Username,<<"','">>,ContactId,<<"')">>]);
			_ ->
			[]	
		end,	


		%%See if contact is registered.
		{_,_, Reg} = ejabberd_odbc:sql_query(?SERVER_IP,
                                [<<"SELECT username FROM username_phone_email WHERE CONCAT(ccode,phone)='">>,ContactId,<<"' OR email='">>,ContactId,<<"'">>]),	
		
		?INFO_MSG("Done w query", []),

		case length(Reg) > 0 of
			true ->

			send_packet_all_resources(<<"w@ce.dev.versapp.co/who">>, <<"will@ce.dev.versapp.co/who">>, build_packet(message_chat, [<<"Helooo">>]));


			false->
			[]
		end,


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

%%%----------------------------------------------------------------------
%%% SUPOPRT METHODS
%%%----------------------------------------------------------------------

contact_exists(Server, Username, ContactId) ->

	%%See if contact is registered.
                {_,_, Reg} = ejabberd_odbc:sql_query(?SERVER_IP,
                                [<<"SELECT username FROM contacts WHERE username='">>,Username,<<"' AND identifier='">>,ContactId,<<"'">>]),

                ?INFO_MSG("Done w query", []),

                length(Reg) > 0.



send_packet_all_resources(FromJIDString, ToJIDString, Packet) ->
    FromJID = jlib:string_to_jid(FromJIDString),
    ToJID = jlib:string_to_jid(ToJIDString),
    ToUser = ToJID#jid.user,
    ToServer = ToJID#jid.server,
    case ToJID#jid.resource of
	<<>> ->
	    send_packet_all_resources(FromJID, ToUser, ToServer, Packet);
	Res ->
	    send_packet_all_resources(FromJID, ToUser, ToServer, Res, Packet)
    end.

send_packet_all_resources(FromJID, ToUser, ToServer, Packet) ->
    case ejabberd_sm:get_user_resources(ToUser, ToServer) of
	[] ->
	    send_packet_all_resources(FromJID, ToUser, ToServer, <<>>, Packet);
	ToResources ->
	    lists:foreach(
	      fun(ToResource) ->
		      send_packet_all_resources(FromJID, ToUser, ToServer,
						ToResource, Packet)
	      end,
	      ToResources)
    end.

send_packet_all_resources(FromJID, ToU, ToS, ToR, Packet) ->
    ToJID = jlib:make_jid(ToU, ToS, ToR),
    ejabberd_router:route(FromJID, ToJID, Packet).


build_packet(message_chat, [Body]) ->
    {xmlel, <<"message">>,
     [{<<"type">>, <<"chat">>}, {<<"id">>, randoms:get_string()}],
     [{xmlel, <<"body">>, [], [{xmlcdata, Body}]},

		#xmlel{name = <<"broadcast">>, attrs = [], children = [#xmlel{ name = <<"type">>, attrs = [], children = [{xmlcdata, <<"new_user">>}]}, #xmlel{ name = <<"username">>, attrs = [], children = [{xmlcdata, <<"username_goes_here">>}]},  #xmlel{ name = <<"full_name">>, attrs = [], children = [{xmlcdata, <<"Name goes here">>}]}   ]}

	%%	#xmlel{name = <<"property">>, attrs = [], children = [#xmlel{ name = <<"name">>, attrs = [], children = [{xmlcdata, <<"time">>}]},  #xmlel{ name = <<"value">>, attrs = [{<<"type">>, <<"string">>}], children = [{xmlcdata, <<"hi">>}]}]}
	
		]};
build_packet(message_headline, [Subject, Body]) ->
    {xmlel, <<"message">>,
     [{<<"type">>, <<"headline">>}, {<<"id">>, randoms:get_string()}],
     [{xmlel, <<"subject">>, [], [{xmlcdata, Subject}]},
      {xmlel, <<"body">>, [], [{xmlcdata, Body}]}
     ]
    }.


is_valid_http_request({request,'POST',_PATH,_Q , _Something, _Undef  , _Lang  , Data , _A, _Host, _P , _T , Header} = Request)->

	[{Key, UserAndKey}] = lists:filter(fun({Key, Val})-> Key=='Authorization' end,Header),

        [_, Auth] = string:tokens(binary_to_list(UserAndKey), " "),

        [Username, AccessKey] = string:tokens(binary_to_list(base64:decode(Auth)), ":"),

	{_,_, Result} = ejabberd_odbc:sql_query(?SERVER_IP,
                                [<<"SELECT * FROM session WHERE username='">>,Username,<<"' AND session_key='">>,AccessKey,<<"'">>]),

	case length(Result) > 0 of
		true->
			Username;
		_->
			false
	end.


