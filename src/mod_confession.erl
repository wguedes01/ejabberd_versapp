-module(mod_confession).
-author('willbrazil.usa@gmail.com').
-version('1.0').

%% Every ejabberd module must implement gen_mod.
%% gen_mod requires two functions: start and stop.
-behavior(gen_mod).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").


-define(NS_CONFESSION, <<"who:iq:confession">>).
-define(DEGREE_FRIEND_1, <<"1">>).
-define(DEGREE_FRIEND_2, <<"2">>).
-define(DEGREE_FRIEND_3, <<"3">>).
-define(DEGREE_GLOBAL, <<"global">>).

-export([start/2, stop/1]).
-export([handle_confession_iq/3]).

%%Methods to interact with database
-export([destroy_confession/3]).
-export([create_confession/3, insert_confession_into_db/3, get_confessions/3, destroy_confession/3]).
-export([toggle_favorite/3, add_favorite/2, delete_favorite/2]).
-export([get_roster_entries/2]).

-export([get_confessions_with_degree/3, build_select_query/4, query_result_to_confession_list/2]).


%%Util
-export([get_timestamp/0]).

-import(mod_offline_post, [dispatch_confession_post/3]).


-import(mod_http_contacts_manager, [send_packet_all_resources/3, build_packet/2]).

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

%%Respond to GET requests.
handle_confession_iq(#jid{user = User, server = Server,
		      resource = Resource} = From,
		 _To, #iq{type = get, sub_el = SubEl} = IQ) ->

	%% get_confessions(From, SubEl, IQ);
	get_confessions_with_degree(From, SubEl, IQ);

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
					%%insert_confession_into_db(From, Tag, IQ);			
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

	JIDString = jlib:jid_to_string(jlib:make_jid(User,Server, <<"">>)),

	{ConfessionId, Username, Body, ImageUrl, CreatedTimestamp, _, _} = insert_confession_into_db(JID, TagEl, IQ),

	Result = string:join([ConfessionId, CreatedTimestamp], ","),

%% SEND THIS 
%%	ejabberd_router:route(JID, JID, {xmlel,<<"iq">>,[{<<"id">>,<<"confession_set">>},{<<"type">>,<<"result">>}],[{xmlel,"value",[],[{xmlcdata,<<"BOOOOOM">>}]}]}),


	?INFO_MSG("THIS IS THE PACKET IM SENDING: ~p",[jlib:iq_to_xml(IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, iolist_to_binary("BOOOOOM")}]}]})]),

	IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, iolist_to_binary(Result)}]}]}.



%%Els is formatted as [{xmlcdata,<<" ">>},{xmlel,<<"body">>,[],[{xmlcdata,<<"Hello, World!">>}]},{xmlcdata,<<" ">>},{xmlel,<<"image_url">>,[],[]},{xmlcdata,<<" ">>}]
insert_confession_into_db(#jid{user = User, server = Server,
                      resource = Resource}, TagEl,IQ) ->

	BodyString = xml:get_subtag_cdata(TagEl, <<"body">>),
	ImageUrlString = xml:get_subtag_cdata(TagEl, <<"image_url">>),
	JIDString = jlib:jid_to_string(jlib:make_jid(User,Server, <<"">>)),
	
	[CreatedTimestamp] = get_timestamp(),

        ejabberd_odbc:sql_query(Server,
                                [<<"INSERT INTO confessions (jid, body, image_url, created_timestamp) VALUES ('">>,User,<<"','">>,BodyString,<<"', '">>,ImageUrlString,<<"', '">>,CreatedTimestamp,<<"')">>]),

       %% get id of confession just inserted.
        {_, _, [[ConfessionIdTerm]]} = ejabberd_odbc:sql_query(Server,
                                [<<"SELECT confession_id FROM confessions WHERE jid='">>,User,<<"' AND created_timestamp='">>,CreatedTimestamp,<<"' ">>]),

	?INFO_MSG("ConfIdTerm, CreatedTimestamp: ~p, ~p", [ConfessionIdTerm, CreatedTimestamp]),
	
	EscapedId = ejabberd_odbc:decode_term(ConfessionIdTerm),

	?INFO_MSG("ConfIdTerm ESCAPED: ~p", [EscapedId]),
	?INFO_MSG("io_lib:format(DecodedId): ~p", [io_lib:format("~p", [EscapedId])]),



	ConfessionIdString = ejabberd_odbc:decode_term(ConfessionIdTerm),


	[Id] = io_lib:format("~p", [ConfessionIdString]),
	Result = string:join([Id, CreatedTimestamp], ","),

	Confession = {Id, JIDString, BodyString, ImageUrlString, CreatedTimestamp, 0, 0},
	
Confession.
%%IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, iolist_to_binary(Result)}]}]}.



get_confessions(#jid{user = User, server = Server,
                      resource = _R} = From, Tag, IQ)->

	MyJIDString = jlib:jid_to_string(jlib:make_jid(User,Server, <<"">>)),

	?INFO_MSG("\n\n GETTING CONFESSIONS", []),

	SinceString = binary_to_list(xml:get_subtag_cdata(Tag, <<"since">>)),


	?INFO_MSG("\n\nSince String: ~p", [SinceString]),


	%% Get number of friends a user has.
	{_,_,NumFriends} = ejabberd_odbc:sql_query(Server,
                              [<<"SELECT username FROM rosterusers WHERE username='">>,User,<<"'">>]),

	?INFO_MSG("\n\nCOUNT: ~p", [NumFriends]),
	?INFO_MSG("\n\nCOUNT: ~p", [length(NumFriends)]),

	?INFO_MSG("\n\nCOUNT: ~p", [length(NumFriends) > 3]),

	Query = case length(NumFriends) > 3 of
		false ->
			string:join([binary_to_list(<<"SELECT confessions.*, GROUP_CONCAT(DISTINCT confession_favorites.jid SEPARATOR ', ') AS favorited_users, count(DISTINCT confession_favorites.jid) AS num_favorites FROM confessions LEFT JOIN confession_favorites ON confessions.confession_id = confession_favorites.confession_id LEFT JOIN rosterusers ON rosterusers.username = confessions.jid WHERE (confessions.created_timestamp > '">>),SinceString,binary_to_list(<<"') GROUP BY confessions.confession_id ORDER BY confessions.created_timestamp ASC LIMIT 100">>)],"");
		_ ->
			?INFO_MSG("\n\nFALSEEEE", []),
			string:join([binary_to_list(<<"SELECT confessions.*, GROUP_CONCAT(DISTINCT confession_favorites.jid SEPARATOR ', ') AS favorited_users, count(DISTINCT confession_favorites.jid) AS num_favorites FROM confessions LEFT JOIN confession_favorites ON confessions.confession_id = confession_favorites.confession_id LEFT JOIN rosterusers ON rosterusers.username = confessions.jid WHERE (confessions.created_timestamp > '">>),SinceString,binary_to_list(<<"') AND ((rosterusers.username = '">>),binary_to_list(User),binary_to_list(<<"' AND rosterusers.subscription = 'B') OR confessions.jid = '">>),binary_to_list(User),binary_to_list(<<"') GROUP BY confessions.confession_id ORDER BY confessions.created_timestamp ASC LIMIT 100">>)],"")
	end,


	?INFO_MSG("\n\nQuery: ~p", [Query]),

	{_, _, Result} = ejabberd_odbc:sql_query(Server, [Query]),


        ?INFO_MSG("Result query for confessions is: ~p", [Result]),

	Filtered = lists:map(fun(ConfessionTerms)-> 
			
			lists:map(fun(Term)-> 
				case Term of
					null ->
						"";
					_ ->
						binary_to_list(Term)
				end
			 end, ConfessionTerms)
	end,Result),

	?INFO_MSG("Result FILTERED: ~p", [Filtered]),	

	

	Filtered2 = lists:map(fun(Confession)-> 
		
		lists:flatten(io_lib:format("~p", [Confession]))			

	end, Filtered),

	?INFO_MSG("Result FILTERED2222: ~p", [Filtered2]),


IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, iolist_to_binary(Filtered2)}]}]}.



get_confessions_with_degree(#jid{user = User, server = Server,
                      resource = _R} = From, Tag, IQ) ->

	?INFO_MSG("\n\n\nBEGIN GET WITH DEGREE", []),

	MyJIDString = jlib:jid_to_string(jlib:make_jid(User,Server, <<"">>)),
	SinceString = binary_to_list(xml:get_subtag_cdata(Tag, <<"since">>)),

	%% Gets degree of connectivity.
	Degree = xml:get_subtag_cdata(Tag, <<"degree">>),


	%% Creates SQL Query that will be executed based on the degree of connectivity provided.
	Query = build_select_query(Degree, binary_to_list(MyJIDString), binary_to_list(User), SinceString),

	?INFO_MSG("\n\n\nTHE QUERY REQUESTED IS: ~p", [Query]),

	{_, _, Result} = ejabberd_odbc:sql_query(Server, [Query]),

	Confessions = query_result_to_confession_list(Result, Degree),

	IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, iolist_to_binary(Confessions)}]}]}.

%% Gets the result of a query and converts it into a formatted list of confessions.
query_result_to_confession_list(Result, Degree) ->

	Filtered = lists:map(fun(ConfessionTerms)->
			
			%% Adds degree of connectivity to the end of the confession set.
	%%		NewConfessionTerms = lists:append(ConfessionTerms, [Degree]),					
			NewConfessionTerms = ConfessionTerms,			

			%% Convert each item in the confession set from binary to list.
                        lists:map(fun(Term)->
                                case Term of
                                        null ->
                                                "";
                                        _ ->
                                                binary_to_list(Term)
                                end
                         end, NewConfessionTerms)
        end,Result),

        ?INFO_MSG("Result FILTERED: ~p", [Filtered]),



        Filtered2 = lists:map(fun(Confession)->

                lists:flatten(io_lib:format("~p", [Confession]))

        end, Filtered),

	Filtered2.

build_select_query(?DEGREE_FRIEND_1, JIDString, Username, SinceString)->

	"SELECT confessions.*, CASE " ++
		"WHEN FIND_IN_SET('" ++ Username ++ "', GROUP_CONCAT(confession_favorites.jid SEPARATOR ',')) > 0 THEN 'YES' " ++ 
		"ELSE 'NO' " ++
	"END AS favorited_users, count(confession_favorites.jid) AS num_favorites, '" ++ binary_to_list(?DEGREE_FRIEND_1) ++ "' AS connection " ++ 
	"FROM confessions " ++ 
	"LEFT JOIN confession_favorites " ++ 
	"ON confessions.confession_id = confession_favorites.confession_id " ++ 
	"WHERE confessions.jid IN " ++ 
		"(SELECT username FROM rosterusers WHERE jid = '" ++ JIDString ++ "') " ++ 
	"OR confessions.jid = '" ++ Username ++ "' " ++ 
	"GROUP BY confessions.confession_id " ++ 
	"ORDER BY confessions.created_timestamp ASC " ++ 
	"LIMIT 100";

build_select_query(?DEGREE_FRIEND_2, JIDString, Username, SinceString)->
	
	"SELECT confessions.*, CASE WHEN FIND_IN_SET('" ++ Username ++ "', GROUP_CONCAT(confession_favorites.jid SEPARATOR ',')) > 0 THEN 'YES' ELSE 'NO' END AS favorited_users, count(confession_favorites.jid) AS num_favorites, '" ++ binary_to_list(?DEGREE_FRIEND_1) ++ "' AS connection " ++ 
	"FROM confessions LEFT JOIN confession_favorites ON confessions.confession_id = confession_favorites.confession_id " ++ 
	"WHERE confessions.jid IN " ++
 		"(SELECT username FROM rosterusers WHERE jid = '" ++ JIDString ++ "') " ++ 
		"OR confessions.jid = '" ++ Username ++ "' " ++
	"GROUP BY confessions.confession_id " ++
	"LIMIT 50 " ++ 
	"UNION SELECT confessions.*, CASE WHEN FIND_IN_SET('" ++ Username ++ "', GROUP_CONCAT(confession_favorites.jid SEPARATOR ',')) > 0 THEN 'YES' ELSE 'NO' END AS favorited_users, count(confession_favorites.jid) AS num_favorites, '" ++ binary_to_list(?DEGREE_FRIEND_2) ++ "' AS connection " ++ 
	"FROM confessions LEFT JOIN confession_favorites ON confessions.confession_id = confession_favorites.confession_id " ++
	"WHERE confessions.jid IN " ++
		"(SELECT DISTINCT username FROM rosterusers WHERE jid IN " ++
			"(SELECT jid FROM rosterusers WHERE username = '" ++ Username ++ "') " ++ 
		"AND username NOT IN " ++
			"(SELECT username FROM rosterusers WHERE jid = '" ++ JIDString ++ "')) " ++
	"AND confessions.jid != '" ++ Username ++ "' " ++ 
	"GROUP BY confessions.confession_id " ++ 
	"ORDER BY created_timestamp ASC LIMIT 100"; 

build_select_query(?DEGREE_FRIEND_3, JIDString, Username, SinceString)->

	"SELECT confessions.*, CASE WHEN FIND_IN_SET('" ++ Username ++ "', GROUP_CONCAT(confession_favorites.jid SEPARATOR ',')) > 0 THEN 'YES' ELSE 'NO' END AS favorited_users, count(confession_favorites.jid) AS num_favorites, '" ++ binary_to_list(?DEGREE_FRIEND_1) ++ "' AS connection " ++ 
	"FROM confessions LEFT JOIN confession_favorites ON confessions.confession_id = confession_favorites.confession_id " ++ 
	"WHERE confessions.jid IN " ++
 		"(SELECT username FROM rosterusers WHERE jid = '" ++ JIDString ++ "') " ++ 
		"OR confessions.jid = '" ++ Username ++ "' " ++
	"GROUP BY confessions.confession_id " ++
	"LIMIT 50 " ++ 
	"UNION SELECT confessions.*, CASE WHEN FIND_IN_SET('" ++ Username ++ "', GROUP_CONCAT(confession_favorites.jid SEPARATOR ',')) > 0 THEN 'YES' ELSE 'NO' END AS favorited_users, count(confession_favorites.jid) AS num_favorites, '" ++ binary_to_list(?DEGREE_FRIEND_2) ++ "' AS connection " ++ 
	"FROM confessions LEFT JOIN confession_favorites ON confessions.confession_id = confession_favorites.confession_id " ++
	"WHERE confessions.jid IN " ++
		"(SELECT DISTINCT username FROM rosterusers WHERE jid IN " ++
			"(SELECT jid FROM rosterusers WHERE username = '" ++ Username ++ "') " ++ 
		"AND username NOT IN " ++
			"(SELECT username FROM rosterusers WHERE jid = '" ++ JIDString ++ "')) " ++
	"AND confessions.jid != '" ++ Username ++ "' " ++ 
	"GROUP BY confessions.confession_id " ++ 
	"LIMIT 25 " ++
	"UNION SELECT confessions.*, CASE WHEN FIND_IN_SET('" ++ Username ++ "', GROUP_CONCAT(confession_favorites.jid SEPARATOR ',')) > 0 THEN 'YES' ELSE 'NO' END AS favorited_users, count(confession_favorites.jid) AS num_favorites, '" ++ binary_to_list(?DEGREE_FRIEND_3) ++ "' AS connection " ++ 
	"FROM confessions LEFT JOIN confession_favorites ON confessions.confession_id = confession_favorites.confession_id WHERE confessions.jid IN " ++
		"(SELECT DISTINCT username FROM rosterusers WHERE jid IN " ++ 
			"(SELECT jid FROM rosterusers WHERE username IN " ++ 
				"(SELECT username FROM rosterusers WHERE jid = '" ++ JIDString ++ "') " ++ 
			"AND jid != '" ++ JIDString ++ "') " ++ 
		"AND username NOT IN " ++ 
			"(SELECT DISTINCT username FROM rosterusers WHERE jid IN " ++ 
				"(SELECT jid FROM rosterusers WHERE username = '" ++ Username ++ "')) " ++ 
		"AND username NOT IN " ++
			"(SELECT username FROM rosterusers WHERE jid = '" ++ JIDString ++ "') " ++ 
		"AND username != '" ++ Username ++ "')  " ++
 	"AND confessions.jid != '" ++ Username ++ "' " ++
	"GROUP BY confessions.confession_id " ++
	"ORDER BY created_timestamp ASC LIMIT 100";

	
%%	"SELECT confessions.*, CASE WHEN FIND_IN_SET('" ++ Username ++ "', GROUP_CONCAT(confession_favorites.jid SEPARATOR ',')) > 0 THEN 'YES' ELSE 'NO' END AS favorited_users, count(confession_favorites.jid) AS num_favorites FROM confessions LEFT JOIN confession_favorites ON confessions.confession_id = confession_favorites.confession_id WHERE confessions.jid IN (SELECT DISTINCT username FROM rosterusers WHERE jid IN (SELECT jid FROM rosterusers WHERE username IN (SELECT username FROM rosterusers WHERE jid = '" ++ JIDString ++ "') AND jid != '" ++ JIDString ++ "') AND username NOT IN (SELECT DISTINCT username FROM rosterusers WHERE jid IN (SELECT jid FROM rosterusers WHERE username = '" ++ Username ++ "') AND username != '" ++ Username ++ "') ) OR confessions.jid = '" ++ Username ++ "' GROUP BY confessions.confession_id ORDER BY confessions.created_timestamp ASC LIMIT 100";

build_select_query(?DEGREE_GLOBAL, JIDString, Username, SinceString)->

	"SELECT confessions.*, CASE WHEN FIND_IN_SET('" ++ Username ++ "', GROUP_CONCAT(DISTINCT confession_favorites.jid SEPARATOR ', ')) > 0 THEN 'YES' ELSE 'NO' END AS favorited_users, count(DISTINCT confession_favorites.jid) AS num_favorites, '" ++ binary_to_list(?DEGREE_GLOBAL) ++ "' AS connection FROM confessions LEFT JOIN confession_favorites ON confessions.confession_id = confession_favorites.confession_id LEFT JOIN rosterusers ON rosterusers.username = confessions.jid WHERE (confessions.created_timestamp > '" ++ SinceString ++ "') GROUP BY confessions.confession_id ORDER BY confessions.created_timestamp ASC LIMIT 100";

build_select_query(_Other, JIDString, Username, SinceString)->
"".



destroy_confession(#jid{user = User, server = Server,
                      resource = Resource}, TagEl,IQ) ->

	MyJIDString = jlib:jid_to_string(jlib:make_jid(User,Server, <<"">>)),

	ConfessionId = xml:get_tag_attr_s(<<"id">>, TagEl),

	ejabberd_odbc:sql_query(Server,
                                [<<"DELETE FROM confessions WHERE confession_id='">>,ConfessionId,<<"' AND jid='">>,User,<<"'">>]),

IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, <<"Confession Destroyed">>}]}]}.


toggle_favorite(#jid{user = User, server = Server,
                      resource = Resource} = JID, TagEl, IQ)->

	JIDString = jlib:jid_to_string(jlib:make_jid(User,Server, <<"">>)),

	ConfessionId = xml:get_tag_attr_s(<<"id">>, TagEl),

        {_,_,Result} = ejabberd_odbc:sql_query(Server,
                                [<<"SELECT confession_id FROM confession_favorites WHERE confession_id='">>,ConfessionId,<<"' AND jid='">>,User,<<"' ">>]),

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
                                [<<"SELECT jid FROM confessions WHERE confession_id='">>,ConfessionId,<<"'">>]),

			?INFO_MSG("Sending favorite alert to ~p", [CreatorUsername]),

			%%CreatorJID = list_to_binary(lists:concat([binary_to_list(CreatorUsername), "@", binary_to_list(Server)])),

			CreatorJID = jlib:string_to_jid(list_to_binary(lists:concat([binary_to_list(CreatorUsername), "@", binary_to_list(Server)]))),

        		send_confession_favorited_notification(ConfessionId, CreatorJID)
	end,
IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, <<"Confession Favortie Toggled">>}]}]}.


add_favorite(#jid{user = User, server = Server,
                      resource = Resource} = JID, ConfessionId) ->

	JIDString = jlib:jid_to_string(jlib:make_jid(User,Server, <<"">>)),	

	ejabberd_odbc:sql_query(Server,
                                [<<"INSERT INTO confession_favorites (jid, confession_id) VALUES ('">>,User,<<"','">>,ConfessionId,<<"')">>]),

ok.


delete_favorite(#jid{user = User, server = Server,
                      resource = Resource} = JID, ConfessionId) ->

	JIDString = jlib:jid_to_string(jlib:make_jid(User,Server, <<"">>)),

	ejabberd_odbc:sql_query(Server,
                                [<<"DELETE FROM confession_favorites WHERE jid='">>,User,<<"' AND confession_id='">>,ConfessionId,<<"'">>]),

ok.

get_roster_entries(Username, Server) ->
	{_,_,RosterEntries} = ejabberd_odbc:sql_query(Server,
                                [<<"SELECT jid FROM rosterusers WHERE username='">>,Username,<<"'">>]),
RosterEntries.


build_notification_packet(Body) ->
    {xmlel, <<"message">>,
     [{<<"type">>, <<"chat">>}, {<<"id">>, randoms:get_string()}],
     [


                #xmlel{name = <<"broadcast">>, attrs = [], 
			children = [
				#xmlel{ name = <<"type">>, attrs = [], children = [{xmlcdata, <<"confession_favorited">>}]},
				#xmlel{ name = <<"content">>, attrs = [], children = [{xmlcdata, list_to_binary(Body)}]}
			]}


                ]}.


send_confession_favorited_notification(ConfessionId, ToJID)->
	
	Server = ToJID#jid.lserver,

%	FavoriteAlertJSON = lists:flatten(io_lib:format("{confession_id: ~s}", [ConfessionId])),

%        send_packet_all_resources(Server, jlib:jid_to_string(ToJID), build_notification_packet(FavoriteAlertJSON)),


	send_confession_favorited_push_notification(ConfessionId, ToJID),

ok.

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
			FavoriteAlertJSON = lists:flatten(io_lib:format("{confession_id: ~s}", [ConfessionId])),
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

get_timestamp()->
{Mega, Secs, _} = now(),
        CurrentTimestamp = io_lib:format("~p", [Mega*1000000 + Secs]),
CurrentTimestamp.

%%%%% XML Handling ============

get_attr(Attr, XData) ->
    case lists:keysearch(Attr, 1, XData) of
        {Key, {_, Value}} -> Value;
        false -> false
    end.
