-module(mod_confession).
-author('willbrazil.usa@gmail.com').
-version('1.0').

%% Every ejabberd module must implement gen_mod.
%% gen_mod requires two functions: start and stop.
-behavior(gen_mod).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include("custom_records.hrl").

-define(CONFESSIONS_TABLE, <<"confessions">>).
-define(CONFESSIONS_TABLE_COLUMN_CONFESSION_ID, <<"confession_id">>).
-define(CONFESSIONS_TABLE_COLUMN_JID, <<"jid">>).
-define(CONFESSIONS_TABLE_COLUMN_BODY, <<"body">>).
-define(CONFESSIONS_TABLE_COLUMN_IMAGE_URL, <<"image_url">>).
-define(CONFESSIONS_TABLE_COLUMN_CREATED_TIMESTAMP, <<"created_timestamp">>).

-export([start/2, stop/1]).
-export([handle_confession_iq/3]).

%%Methods to interact with database
-export([destroy_confession/3]).
-export([create_confession/3, destroy_confession/3]).
-export([toggle_favorite/3, add_favorite/2, delete_favorite/2]).

-import(mod_offline_post, [dispatch_confession_post/3]).

-import(mod_http_contacts_manager, [send_packet_all_resources/3, build_packet/2]).

-import(custom_odbc_queries, [insert_confession/2]).

start(Host, Opts) ->

	?INFO_MSG("START MOD_CONFESSION. ~p ~p",[Host, Opts]),
	
	IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
	gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_CONFESSION, ?MODULE, handle_confession_iq, IQDisc),

	?INFO_MSG("STARTED MOD_CONFESSION",[]).

stop(Host) ->
	gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_CONFESSION).


%%Respond to SET requests.
handle_confession_iq(#jid{user = User, server = Server,
                      resource = Resource} = From,
                 _To, #iq{type = set, sub_el = SubEl} = IQ) ->

	JIDString = jlib:jid_to_string(jlib:make_jid(User,Server, <<"">>)),

	#xmlel{children = SubEls} = SubEl,

	IQResponse = case xml:remove_cdata(SubEls) of
		[#xmlel{name = Name, attrs = Attrs, children = Els} = Tag| Rest] ->
			?INFO_MSG("\n\nName of tag is: ~p", [Name]),
			case Name of
				<<"create">> ->
					create_confession(From, Tag, IQ);
				<<"destroy">> ->
					destroy_confession(From, Tag, IQ);
				<<"toggle_favorite">> ->
					toggle_favorite(From, Tag, IQ)
			end;
		_ ->
			IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, <<"Action Does Not Exist">>}]}]}
	end,

IQResponse.


create_confession(#jid{user = User, server = Server,
                      resource = Resource} = JID, TagEl, IQ) ->

	Body = xml:get_subtag_cdata(TagEl, <<"body">>),
        ImageUrl = xml:get_subtag_cdata(TagEl, <<"image_url">>),
	
	Confession = custom_odbc_queries:insert_confession(Server, #confession{username = User, body = Body, image_url = ImageUrl}),

	?INFO_MSG("\n\nConfession: ~p", [Confession]),

	Result = string:join([binary_to_list(Confession#confession.id), binary_to_list(Confession#confession.created_timestamp)], ","),


IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, iolist_to_binary(Result)}]}]}.


destroy_confession(#jid{user = User, server = Server,
                      resource = Resource}, TagEl,IQ) ->

	MyJIDString = jlib:jid_to_string(jlib:make_jid(User,Server, <<"">>)),

	ConfessionId = xml:get_tag_attr_s(<<"id">>, TagEl),

	ejabberd_odbc:sql_query(Server,
                                [<<"DELETE FROM ">>,?CONFESSIONS_TABLE,<<" WHERE ">>,?CONFESSIONS_TABLE_COLUMN_CONFESSION_ID,<<"='">>,ConfessionId,<<"' AND jid='">>,User,<<"'">>]),

IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, <<"Confession Destroyed">>}]}]}.


toggle_favorite(#jid{user = User, server = Server,
                      resource = Resource} = JID, TagEl, IQ)->

	JIDString = jlib:jid_to_string(jlib:make_jid(User,Server, <<"">>)),

	ConfessionId = xml:get_tag_attr_s(<<"id">>, TagEl),

        {_,_,Result} = ejabberd_odbc:sql_query(Server,
                                [<<"SELECT ">>,?CONFESSIONS_TABLE_COLUMN_CONFESSION_ID,<<" FROM confession_favorites WHERE ">>,?CONFESSIONS_TABLE_COLUMN_CONFESSION_ID,<<"='">>,ConfessionId,<<"' AND jid='">>,User,<<"' ">>]),

        case (length(Result) > 0) of
                true ->
			?INFO_MSG("REMOVE favorite!!!", []),
                        delete_favorite(JID, ConfessionId);
                false ->
			?INFO_MSG("Adding favorite!!!", []),
                        add_favorite(JID, ConfessionId),
                      %%  send_confession_alert(Server, ConfessionId)

			%% Get username of person who created confession.
			{_,_,[[CreatorUsername]]} = ejabberd_odbc:sql_query(Server,
                                [<<"SELECT jid FROM ">>,?CONFESSIONS_TABLE,<<" WHERE ">>,?CONFESSIONS_TABLE_COLUMN_CONFESSION_ID,<<"='">>,ConfessionId,<<"'">>]),

			?INFO_MSG("Sending favorite alert to ~p", [CreatorUsername]),

			%%CreatorJID = list_to_binary(lists:concat([binary_to_list(CreatorUsername), "@", binary_to_list(Server)])),

			CreatorJID = jlib:string_to_jid(list_to_binary(lists:concat([binary_to_list(CreatorUsername), "@", binary_to_list(Server)]))),

        		send_confession_favorited_push_notification(ConfessionId, CreatorJID)
	end,
IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, <<"Confession Favortie Toggled">>}]}]}.


add_favorite(#jid{user = User, server = Server,
                      resource = Resource} = JID, ConfessionId) ->

	JIDString = jlib:jid_to_string(jlib:make_jid(User,Server, <<"">>)),	

	ejabberd_odbc:sql_query(Server,
                                [<<"INSERT INTO confession_favorites (jid, ">>,?CONFESSIONS_TABLE_COLUMN_CONFESSION_ID,<<") VALUES ('">>,User,<<"','">>,ConfessionId,<<"')">>]),

ok.


delete_favorite(#jid{user = User, server = Server,
                      resource = Resource} = JID, ConfessionId) ->

	JIDString = jlib:jid_to_string(jlib:make_jid(User,Server, <<"">>)),

	ejabberd_odbc:sql_query(Server,
                                [<<"DELETE FROM confession_favorites WHERE jid='">>,User,<<"' AND ">>,?CONFESSIONS_TABLE_COLUMN_CONFESSION_ID,<<"='">>,ConfessionId,<<"'">>]),

ok.


build_notification_packet(Body) ->
    {xmlel, <<"message">>,
     [{<<"type">>, <<"headline">>}, {<<"id">>, randoms:get_string()}],
     [


                #xmlel{name = <<"broadcast">>, attrs = [], 
			children = [
				#xmlel{ name = <<"type">>, attrs = [], children = [{xmlcdata, <<"confession_favorited">>}]},
				#xmlel{ name = <<"confession">>, attrs = [], children = [#xmlel{ name = <<"id">>, attrs = [], children = [{xmlcdata, list_to_binary(Body)}]}]}
			]}


                ]}.

send_confession_favorited_push_notification(ConfessionId, ToJID)->

	%% If last dispatch was 30 min ago, push notification.
	{_,_, LastSentTimestampResult} = ejabberd_odbc:sql_query(ToJID#jid.lserver,
                            [<<"SELECT last_push_timestamp FROM last_push_notifications WHERE username='">>, ToJID#jid.luser ,<<"'">>]),


	CurrentTime = element(1, now()) * 10000 + element(2, now()),
	
	Action = case LastSentTimestampResult of
			[] ->
				{true, send_and_insert};
			[[LastSentTime]] ->
				case (CurrentTime - binary_to_integer(LastSentTime)) > 600 of
					true -> 
						{true, send_and_update};
					false ->
						{false, do_not_send}
				end
		end,

	?INFO_MSG("\n\n\nACTIPN: ~p - ~p", [LastSentTimestampResult, Action]),

	{ShouldSend, _} = Action,

	case ShouldSend of
		false ->
			[];
		true ->
			FavoriteAlertJSON = lists:flatten(io_lib:format("~s", [ConfessionId])),
			send_packet_all_resources(ToJID#jid.lserver, jlib:jid_to_string(ToJID), build_notification_packet(FavoriteAlertJSON)),
			dispatch_confession_post(ToJID, <<"Someone favorited your thought">>, ConfessionId)
	end,

	case Action of
		{_, send_and_insert} ->
			Res = ejabberd_odbc:sql_query(ToJID#jid.lserver,
                            [<<"INSERT INTO last_push_notifications (username, last_push_timestamp) VALUES ('">>, ToJID#jid.luser ,<<"', ">>, integer_to_list(CurrentTime) ,<<")">>]),

			?INFO_MSG("\n\nInsert: ~p", [Res]);
		{_, send_and_update} ->
			Res = ejabberd_odbc:sql_query(ToJID#jid.lserver,
                            [<<"UPDATE last_push_notifications set last_push_timestamp=">>, integer_to_list(CurrentTime) ,<<" WHERE username='">>, ToJID#jid.luser ,<<"'">>]),

			?INFO_MSG("\n\nInsert: ~p", [Res]);
		{false, _} ->
			[]
	end,	

ok.

