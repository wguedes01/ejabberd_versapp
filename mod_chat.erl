-module(mod_chat).
-author('willbrazil.usa@gmail.com').
-version('1.0').

%% Every ejabberd module must implement gen_mod.
%% gen_mod requires two functions: start and stop.
-behavior(gen_mod).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").


-define(NS_CHAT, <<"who:iq:chat">>).

-export([handle_local_iq/3]).
-export([get_chats/3, create_chat/4, get_chat/3, leave_chat/3]).
-export([add_participant/5, get_all_participants/3, handle_participant_set_request/4, update_participant/5, get_participant/3]).
-export([start/2, stop/1, start_chat_mod/3, get_chat_participants/2]).

-export([on_user_send_packet/3]).

start(Host, Opts) ->

	?INFO_MSG("STARTED MOD_CHAT",[]),

	IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),

	?INFO_MSG("Registering on_user_send_packet for chat", []),
	ejabberd_hooks:add(user_send_packet, Host, ?MODULE, on_user_send_packet, 4),

	
	gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_CHAT, ?MODULE, handle_local_iq, IQDisc),
	ok.

stop(Host) ->

	?INFO_MSG("STOPED MOD_CHAT",[]),

	ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, on_user_send_packet, 4),
	
	gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_CHAT),
	ok.


handle_local_iq(#jid{user = User, server = Server,
                      resource = Resource} = From,
                 _To, #iq{type = get, sub_el = SubEl} = IQ) ->

	?INFO_MSG("SubEl: ~p", [SubEl]),


	#xmlel{children = SubEls} = SubEl,
        IQRes = case xml:remove_cdata(SubEls) of
                [#xmlel{name = Name, attrs = Attrs, children = Els} = Tag| Rest] ->
                        case Name of
                                <<"id">> ->
					IdCData = xml:get_subtag_cdata(SubEl, <<"id">>),
					get_chat(IdCData, From, IQ);
				<<"status">> ->
					Status = xml:get_subtag_cdata(SubEl, <<"status">>),
					get_chats(Status, From, IQ);
				<<"participants">> ->
					ChatId = xml:get_tag_attr_s(<<"id">>, Tag),
					?INFO_MSG("\n\nChatId: ~p.\n\n", [ChatId]),
					get_all_participants(From, ChatId, IQ)
                        end;
                _ ->
                        IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, <<"Action Does Not Exist">>}]}]}
        end,

%%	IdCData = xml:get_subtag_cdata(SubEl, <<"id">>),
%%	IQRes = case IdCData of
%%		<<>> -> %%null
%%		Status = xml:get_subtag_cdata(SubEl, <<"status">>),
%%
  %%      	get_chats(Status, From, IQ);
%%		_ -> %%search based on ID
%%		get_chat(IdCData, From, IQ)		
%%	end,


IQRes;
handle_local_iq(#jid{user = User, server = Server,
                      resource = Resource} = From,
                 _To, #iq{type = set, sub_el = SubEl} = IQ) ->

	?INFO_MSG("\n\nSubEl: ~p", [SubEl]),

	#xmlel{children = SubEls} = SubEl,
	IQResponse = case xml:remove_cdata(SubEls) of
                [#xmlel{name = Name, attrs = Attrs, children = Els} = Tag| Rest] ->
                        case Name of
                                <<"create">> ->
					Type = xml:get_subtag_cdata(Tag, <<"type">>),
                                	create_chat(From, Type, Tag, IQ);
 				<<"participant">> ->
					Type = xml:get_subtag_cdata(Tag, <<"status">>),
					?INFO_MSG("\n\nNew status is: ~p",[Type]),
					handle_participant_set_request(From, Type, Tag, IQ)
                        end;
                _ ->
                        IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, <<"Action Does Not Exist">>}]}]}
        end,

IQResponse.

create_chat(#jid{user = User, server = Server,
                      resource = Resource} = From, <<"chat">> = ChatType, TagEl, IQ) ->
	
	%%Check if user being invited has blocked creator of chat. If so, return 'blocked' in IQ query.	


	ChatId = binary_to_list(xml:get_subtag_cdata(TagEl, <<"id">>)),
	ChatName = binary_to_list(xml:get_subtag_cdata(TagEl, <<"name">>)),
	Degree = binary_to_list(xml:get_subtag_cdata(TagEl, <<"degree">>)), %% dEGREE OF CONNECTION
	OwnerId = binary_to_list(xml:get_subtag_cdata(TagEl, <<"owner_id">>)),
	#xmlel{children = Participants} = xml:get_subtag(TagEl, <<"participants">>),

        CreatedTimestamp = get_timestamp(),

	?INFO_MSG("Participants: ~p", [Participants]),	

	SingleParticipant = [Participants],

%%	BREAK HERE
	?INFO_MSG("Single Participant: ~p", [SingleParticipant]),

	%%Creates Chat.
	ejabberd_odbc:sql_query(Server,
                                [<<"INSERT INTO chat (id, type, owner_id, name, created, degree) VALUES
                                         ('">>,ChatId,<<"', '">>,ChatType,<<"','">>, OwnerId, <<"','">>,ChatName,<<"','">>,CreatedTimestamp,<<"', '">>,Degree,<<"')">>]),

	?INFO_MSG("\nParticipants: ~p", [Participants]),

	%%Because it's one to one. Invite oneself.
	add_participant(From, ChatId, User, <<"active">>, CreatedTimestamp),
	
	PList = lists:foreach(fun({xmlel,<<"participant">>,[],[{xmlcdata, Value}]})->

		add_participant(From, ChatId, Value, <<"active">>, CreatedTimestamp),
		?INFO_MSG("Participant Item: ~p", [binary_to_list(Value)])
		
	 end, Participants),
	

	?INFO_MSG("\n\nChat Id: ~p", [ChatId]),
	?INFO_MSG("\nChat Name: ~p", [ChatName]),
	?INFO_MSG("\nOwner Id: ~p", [OwnerId]),
	?INFO_MSG("\nParticipants: ~p", [PList]),

%%ejabberd_odbc:sql_query(Server,
          %%                      [<<"INSERT INTO chat (id, type, owner_id, name, created) VALUES
            %%                             ('">>,ChatId,<<"', '">>,ChatType,<<"','">>, ChatOwner, <<"','">>,ChatName,<<"','">>,CreatedTimestamp,<<"')">>]),

	IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, <<"Creating one to one">>}]}]};
create_chat(#jid{user = User, server = Server,
                      resource = Resource} = From, <<"groupchat">> = ChatType, TagEl, IQ) ->

	ChatId = binary_to_list(xml:get_subtag_cdata(TagEl, <<"id">>)),
        ChatName = binary_to_list(xml:get_subtag_cdata(TagEl, <<"name">>)),
        OwnerId = binary_to_list(xml:get_subtag_cdata(TagEl, <<"owner_id">>)),
        #xmlel{children = Participants} = xml:get_subtag(TagEl, <<"participants">>),

        CreatedTimestamp = get_timestamp(),


        %%Creates Chat.
        ejabberd_odbc:sql_query(Server,
                                [<<"INSERT INTO chat (id, type, owner_id, name, created) VALUES
                                         ('">>,ChatId,<<"', '">>,ChatType,<<"','">>, OwnerId, <<"','">>,ChatName,<<"','">>,CreatedTimestamp,<<"')">>]),

	JoinedTimestamp = <<>>,

        %%Because it's one to one. Invite oneself.
        add_participant(From, ChatId, User, <<"active">>, CreatedTimestamp),

        PList = lists:foreach(fun({xmlel,<<"participant">>,[],[{xmlcdata, Value}]})->

                add_participant(From, ChatId, Value, <<"pending">>, JoinedTimestamp),
                ?INFO_MSG("Participant Item: ~p", [binary_to_list(Value)])

         end, Participants),


	IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, <<"Creating group chat">>}]}]};
create_chat(#jid{user = User, server = Server,
                      resource = Resource} = From, Else, TagEl, IQ)->

IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, <<"Chat TYPE does not exist.">>}]}]}.


handle_participant_set_request(#jid{user = User, server = Server,
                      resource = Resource} = From, <<"active">> = Status, TagEl, IQ) ->
	
	JoinedTimestamp = get_timestamp(),
	ChatId = binary_to_list(xml:get_subtag_cdata(TagEl, <<"chat_id">>)),
	ParticipantUsername = User,
	ParticipantStatus = Status,

	?INFO_MSG("\n\nChatId is to join: ~p", [ChatId]),

	update_participant(From, ChatId, ParticipantUsername, ParticipantStatus, JoinedTimestamp),

	IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, <<"Accepted group invitation">>}]}]};
handle_participant_set_request(#jid{user = User, server = Server,
                      resource = Resource} = From, <<"pending">> = Status, TagEl, IQ) ->


	ChatId = binary_to_list(xml:get_subtag_cdata(TagEl, <<"chat_id">>)),
	ParticipantId = binary_to_list(xml:get_subtag_cdata(TagEl, <<"user_id">>)),

	[Participant] = get_participant(From, ChatId, ParticipantId),

	?INFO_MSG("Participant query is: ~p", [Participant]),

	case Participant of

		false ->
			add_participant(From, ChatId, ParticipantId, <<"pending">>, <<>>);	%%insert participant with pending status
		[Username, InvitedTimestamp, _, _, ParticipantStatus] ->

			case ParticipantStatus of 
				<<"pending">> ->
					%%Do nothing cause user has already been invited to that group.
					[];
				<<"active">> ->
					%%Do nothing because user is already active in the group.
					[];
				<<"inactive">> ->
					%%User has joined and left the group. Make user pending again. If user wants not to receive invitations for that group anymore, he must block the group.
   					ejabberd_odbc:sql_query(Server,
                                        [<<"UPDATE participants SET status='pending', invited_timestamp='">>,InvitedTimestamp,<<"', invited_by='">>,User,<<"', joined_timestamp='' WHERE chat_id='">>,ChatId,<<"' AND username='">>,Username,<<"'">>]);
				<<"blocked">> ->
					%%TODO
					[]
			end

	end,

	%%If participant is inactive in group, chane to pending. If participant is not in group, insert participant as pending.
	


	IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, <<"SET PARTICIPANT RECEIVED">>}]}]};
handle_participant_set_request(#jid{user = User, server = Server,
                      resource = Resource} = From, <<"inactive">> = Status, TagEl, IQ) ->

	ChatId = binary_to_list(xml:get_subtag_cdata(TagEl, <<"chat_id">>)),
	leave_chat(From, ChatId, User),
	IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, <<"Left Chat">>}]}]};
handle_participant_set_request(#jid{user = User, server = Server,
                      resource = Resource} = From, Status, TagEl, IQ) ->
IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, <<"Action not available">>}]}]}.

add_participant(#jid{user = User, server = Server, resource = _R}, ChatId, ParticipantUsername, ParticipantStatus, JoinedTimestamp) ->


	?INFO_MSG("\n\nID OF PERSON BEING INVITED: ~p", ParticipantUsername),
	ejabberd_odbc:sql_query(Server,
                                        [<<"INSERT INTO participants (chat_id, username, invited_timestamp, joined_timestamp, invited_by, status) VALUES
                                                 ('">>,ChatId,<<"', '">>,ParticipantUsername,<<"','">>, get_timestamp(), <<"','">>,JoinedTimestamp,<<"','">>,User,<<"', '">>, ParticipantStatus,<<"')">>]),

	%%SEND INVITATION TO PARTICIPANT.

ok.


get_participant(#jid{user = User, server = Server, resource = _R}, ChatId, ParticipantUsername) ->

	{_, _, Result} = ejabberd_odbc:sql_query(Server,
                             [<<"SELECT username, invited_timestamp, joined_timestamp, invited_by, status FROM participants WHERE chat_id='">>,ChatId,<<"' AND username='">>,ParticipantUsername,<<"'">>]),

	Participant = case Result of
		[] ->
			false;
		_ ->
		Result
	end,

Participant.

update_participant(#jid{user = User, server = Server, resource = _R}, ChatId, ParticipantUsername, ParticipantStatus, JoinedTimestamp) ->

	?INFO_MSG("About to update particpant: ~p ~p ~p ~p" ,[ParticipantUsername, ParticipantStatus, ChatId, JoinedTimestamp]),
	ejabberd_odbc:sql_query(Server,
                                        [<<"UPDATE participants SET status='">>,ParticipantStatus,<<"', joined_timestamp='">>,JoinedTimestamp,<<"' WHERE chat_id='">>,ChatId,<<"' AND username='">>,ParticipantUsername,<<"'">>]),

ok.

%%When a participant leaves that chat, the entry is not deleted from DB. Instead, the status of that participant becomes inactive.
leave_chat(#jid{user = User, server = Server, resource = _R}, ChatId, ParticipantUsername) ->

	?INFO_MSG("ChatId: ~p. UName: ~p", [ChatId, ParticipantUsername]),

	ejabberd_odbc:sql_query(Server,
                                        [<<"UPDATE participants SET status='inactive' WHERE chat_id='">>,ChatId,<<"' AND username='">>,ParticipantUsername,<<"'">>]),

ok.


get_chat(ChatIdTerm, #jid{user = User, server = Server,
                      resource = Resource}, IQ) ->

	{_, _, Result} = ejabberd_odbc:sql_query(Server,
                             [<<"SELECT GROUP_CONCAT(participants.username separator ', ') AS participants, chat.*
                                         FROM participants LEFT JOIN chat ON participants.chat_id = chat.id 
                                        WHERE chat_id = '">>,ChatIdTerm,<<"'  && username != '">>,User,<<"' GROUP BY chat.id;">>]),


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


        Filtered2 = lists:map(fun(Confession)->

                lists:flatten(io_lib:format("~p", [Confession]))

        end, Filtered),



IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, iolist_to_binary(Filtered2)}]}]}.

get_chats(<<"active">>, #jid{user = User, server = Server,
                      resource = Resource}, IQ) ->

	{_, _, Result} = ejabberd_odbc:sql_query(Server,
                             [<<"SELECT GROUP_CONCAT(participants.username separator ', ') AS participants, chat.*
                                         FROM participants LEFT JOIN chat ON participants.chat_id = chat.id 
                                        WHERE chat_id = ANY (SELECT chat_id FROM participants 
                                        WHERE username = '">>,User,<<"' && participants.status = 'active')  && username != '">>,User,<<"' GROUP BY chat.id;">>]),

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


	Filtered2 = lists:map(fun(Confession)->

                lists:flatten(io_lib:format("~p", [Confession]))

        end, Filtered),


	IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, iolist_to_binary(Filtered2)}]}]};
get_chats(<<"pending">>, #jid{user = User, server = Server,
                      resource = Resource}, IQ) ->

	{_, _, Result} = ejabberd_odbc:sql_query(Server,
                             [<<"SELECT GROUP_CONCAT(participants.username separator ', ') AS participants, chat.*
                                         FROM participants LEFT JOIN chat ON participants.chat_id = chat.id 
                                        WHERE chat_id = ANY (SELECT chat_id FROM participants 
                                        WHERE username = '">>,User,<<"' && participants.status = 'pending')  && username != '">>,User,<<"' GROUP BY chat.id;">>]),

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


        Filtered2 = lists:map(fun(Confession)->

                lists:flatten(io_lib:format("~p", [Confession]))

        end, Filtered),


        IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, iolist_to_binary(Filtered2)}]}]};
get_chats(Else, #jid{user = User, server = Server,
                      resource = Resource}, IQ) ->
IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, <<"TYPE DOES NOT EXIST">>}]}]}.


get_all_participants(#jid{user = User, server = Server,
                      resource = Resource}, ChatId, IQ) ->

	{_,_, Result} = ejabberd_odbc:sql_query(Server,
                                [<<"select username, invited_by, status from participants WHERE chat_id='">>,ChatId,<<"'">>]),

	Filtered = lists:map(fun(ParticipantTerms)->

                        lists:map(fun(Term)->
                                case Term of
                                        null ->
                                                "";
                                        _ ->
                                                binary_to_list(Term)
                                end
                         end, ParticipantTerms)
        end,Result),


        Filtered2 = lists:map(fun(Participant)->

                lists:flatten(io_lib:format("~p", [Participant]))

        end, Filtered),	


IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, iolist_to_binary(Filtered2)}]}]}.


start_chat_mod({Item, User, Server, _, _, _, _} = From, To,  #iq{type = Type, lang = Lang, sub_el = SubEl} = IQ) ->

	{xmlelement,"query",  _, [{xmlelement,"chat",MyAttrs,_}]} = SubEl,

	?INFO_MSG("MyAttrs: ~p", [MyAttrs]),

	Action = get_attr("action", MyAttrs),
	ChatId = get_attr("id", MyAttrs),

	{Mega, Secs, _} = now(),
	CurrentTimestamp = io_lib:format("~p", [Mega*1000000 + Secs]),

	case {Type, Action, ChatId} of
                %% Get all chats current user owns
		%%{get, _, MyChatId} when MyChatId == false ->
                        
		%%	OwnerId = From, %%Make this get the current's user JID. just username

		%%	{_, _, Result} = ejabberd_odbc:sql_query(Server,
                %%             [<<"select * from chat WHERE owner_id='">>,OwnerId,<<"'">>]),
		
		%%FinalResult = lists:append([Result]),
	
	
		%%	IQ#iq{type = result, sub_el = [{xmlelement, "value", [], [{xmlcdata, lists:flatten(io_lib:format("~p", [FinalResult]))}]}]};
		%% Get where id =
		{get, Execute, MyChatId} when Execute == "get_chat" ->

			{_, _, Result} = ejabberd_odbc:sql_query(Server,
                              [<<"select * from chat WHERE id='">>, MyChatId,<<"'">>]),

                         IQ#iq{type = result, sub_el = [{xmlelement, "value", [], [{xmlcdata, lists:flatten(io_lib:format("~p", [Result]))}]}]};
		%% Get chat's user is joined to.
        	{get, Execute, MyChatId} when Execute == "joined" ->

                    %%    {_, _, Result} = ejabberd_odbc:sql_query(Server,
                  %%            [<<"select chat.* from chat, participants WHERE chat.id=participants.chat_id AND participants.username='">>,User,<<"' AND 
		%%										(participants.joined_timestamp IS NOT NULL AND participants.joined_timestamp!='') AND
			%%										participants.status!='inactive'">>]),
			
			{_, _, Result} = ejabberd_odbc:sql_query(Server,
                             [<<"SELECT GROUP_CONCAT(participants.username separator ', ') AS participants, chat.*
					 FROM participants LEFT JOIN chat ON participants.chat_id = chat.id 
					WHERE chat_id = ANY (SELECT chat_id FROM participants 
					WHERE username = '">>,User,<<"' && participants.status = 'active')  && username != '">>,User,<<"' GROUP BY chat.id;">>]),

			?INFO_MSG("RESULT IS: ~p", [Result]),

                         IQ#iq{type = result, sub_el = [{xmlelement, "value", [], [{xmlcdata, lists:flatten(io_lib:format("~p", [Result]))}]}]};
		%% Get chat's user has been invited but has not yet accepted.
                {get, Execute, MyChatId} when Execute == "pending" ->

			{_, _, Result} = ejabberd_odbc:sql_query(Server,
			[<<"select chat.* from chat, participants WHERE chat.id=participants.chat_id AND participants.username='">>,User,<<"' AND
                                                                                                (participants.joined_timestamp IS NULL OR participants.joined_timestamp='')">>]),
                         IQ#iq{type = result, sub_el = [{xmlelement, "value", [], [{xmlcdata, lists:flatten(io_lib:format("~p", [Result]))}]}]};
		%% Get participants of a group
                {get, Execute, MyChatId} when Execute == "get_participants" ->

                        Result = get_chat_participants(Server, MyChatId),
                        ?INFO_MSG("RESULT IS: ~p", [Result]),
			 IQ#iq{type = result, sub_el = [{xmlelement, "value", [], [{xmlcdata, lists:flatten(io_lib:format("~p", [Result]))}]}]};
		{set, Execute, _} when Execute == "create" ->
			
		        {_, ChatType} = xml:get_attr("type", MyAttrs),
		        {_, ChatOwner} = xml:get_attr("owner_id", MyAttrs),
       			{_, ChatName} = xml:get_attr("name", MyAttrs),
		%%	{_, CreatedTimestamp} = xml:get_attr("created_timestamp", MyAttrs),
			CreatedTimestamp = CurrentTimestamp,			
		
			ejabberd_odbc:sql_query(Server,
      				[<<"INSERT INTO chat (id, type, owner_id, name, created) VALUES
					 ('">>,ChatId,<<"', '">>,ChatType,<<"','">>, ChatOwner, <<"','">>,ChatName,<<"','">>,CreatedTimestamp,<<"')">>]),
                        IQ#iq{type = result, sub_el = [{xmlelement, "value", [], [{xmlcdata, "Creating..."}]}]};
		{set, Execute, MyChatId} when Execute == "destroy" ->

			ejabberd_odbc:sql_query(Server,
                              [<<"delete from chat WHERE id='">>, MyChatId,<<"'">>]),

			ejabberd_odbc:sql_query(Server,
                              [<<"delete from participants WHERE chat_id='">>, MyChatId,<<"'">>]),

                        IQ#iq{type = result, sub_el = [{xmlelement, "value", [], [{xmlcdata, "destroying.."}]}]};
		{set, Execute, _} when Execute == "update" ->
                        IQ#iq{type = result, sub_el = [{xmlelement, "value", [], [{xmlcdata, "Updating"}]}]};
		{set, Execute, MyChatId} when Execute == "participant_insert" ->
			
			{_, InvitedUser} = xml:get_attr("invited_username", MyAttrs),
                       %% {_, InvitedTimestamp} = xml:get_attr("invited_timestamp", MyAttrs),
			InvitedTimestamp = CurrentTimestamp,
                        JoinedTimestamp = "",
                        InvitedBy = User,

			%% If participant is inserted into One2One, automaticlly accept.
			{_, _, ChatList} = ejabberd_odbc:sql_query(Server,
                        [<<"select chat.* from chat WHERE id='">>,MyChatId,<<"' AND type='chat'">>]),

			IsOneToOne = length(ChatList) > 0,

			case IsOneToOne of
				false ->
					ejabberd_odbc:sql_query(Server,
	                                [<<"INSERT INTO participants (chat_id, username, invited_timestamp, joined_timestamp, invited_by) VALUES ('">>,MyChatId,<<"', '">>,InvitedUser,<<"','">>, InvitedTimestamp, <<"','">>,JoinedTimestamp,<<"','">>,InvitedBy,<<"')">>]);
				true ->
					ejabberd_odbc:sql_query(Server,
	                                [<<"INSERT INTO participants (chat_id, username, invited_timestamp, joined_timestamp, invited_by, status) VALUES
        	                                 ('">>,MyChatId,<<"', '">>,InvitedUser,<<"','">>, InvitedTimestamp, <<"','">>,CurrentTimestamp,<<"','">>,InvitedBy,<<"', 'active')">>])
			end,					


			IQ#iq{type = result, sub_el = [{xmlelement, "value", [], [{xmlcdata, "Adding parcitipant"}]}]};
		{set, Execute, MyChatId} when Execute == "participant_join" ->

			%%{_, JoinedTimestamp} = xml:get_attr("joined_timestamp", MyAttrs),
			JoinedTimestamp = CurrentTimestamp,	

                        ejabberd_odbc:sql_query(Server,
                                [<<"UPDATE participants SET joined_timestamp='">>,JoinedTimestamp,<<"' , status='active' WHERE chat_id='">>,MyChatId,<<"' AND username='">>,User,<<"'">>]),

                        IQ#iq{type = result, sub_el = [{xmlelement, "value", [], [{xmlcdata, "Joining group"}]}]};
		{set, Execute, MyChatId} when Execute == "participant_leave" ->

                        ejabberd_odbc:sql_query(Server,
                              [<<"delete from participants WHERE chat_id='">>, MyChatId,<<"' AND username='">>,User,<<"'">>]),

                        IQ#iq{type = result, sub_el = [{xmlelement, "value", [], [{xmlcdata, "Leaving group"}]}]};
		{set, Execute, MyChatId} when Execute == "participant_update_status" ->

			{_, ParticipantId} = xml:get_attr("participant_id", MyAttrs),
                        {_, Status} = xml:get_attr("status", MyAttrs),				

                        ejabberd_odbc:sql_query(Server,
                              [<<"UPDATE participants SET status='">>,Status,<<"' WHERE chat_id='">>,MyChatId,<<"' AND username='">>,ParticipantId,<<"'">>]),

			%% If user just 'inactivate' a user, check if there are active users still. If not, remove chat and its participants.
			if
                                Status == "inactive" ->
                                	?INFO_MSG("BOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOMM", []),
					{_, _, ActiveParticipants} = ejabberd_odbc:sql_query(Server,
                        	        [<<"select status from participants WHERE chat_id='">>,MyChatId,<<"' AND status='active'">>]),
					?INFO_MSG("PARTICIPANTS: ~p", [ActiveParticipants]),

					Size = length(ActiveParticipants),

					if
						Size == 0 ->		
							ejabberd_odbc:sql_query(Server,
			                	             [<<"delete from chat WHERE id='">>, MyChatId,<<"'">>]),

	                       				ejabberd_odbc:sql_query(Server,
				                              [<<"delete from participants WHERE chat_id='">>, MyChatId,<<"'">>]);
						true -> 
							?INFO_MSG("There are still active participants", [])
					end;
				true ->
					?INFO_MSG("Does nothing", [])
                        end,

                        IQ#iq{type = result, sub_el = [{xmlelement, "value", [], [{xmlcdata, "Leaving group"}]}]};
		_ ->
			IQ#iq{type = result, sub_el = [{xmlelement, "value", [], [{xmlcdata, "INVALID CHAT ARGUMENTS"}]}]}
	end.


%%%=======================
%%%% XML processing

get_attr(Attr, XData) ->
    case lists:keysearch(Attr, 1, XData) of
        {Key, {_, Value}} -> Value;
        false -> false
    end.


get_chat_participants(Server, ChatId) ->
	{_,_, Result} = ejabberd_odbc:sql_query(Server,
                                [<<"select username, status from participants WHERE chat_id='">>,ChatId,<<"' AND (joined_timestamp IS NOT NULL AND joined_timestamp!='')">>]),
Result.

get_timestamp()->

	{Mega, Secs, _} = now(),
        CreatedTimestamp = io_lib:format("~p", [Mega*1000000 + Secs]),
	CreatedTimestamp.



on_user_send_packet(#jid{user = User, server = Server,
                      resource = Resource} = From, #jid{user = ToUser, server = ToServer, resource = _R2} = To, #xmlel{name = <<"message">>, attrs = Attrs, children = Children} = Packet) ->

        %% Check if user is blocked before turning chat active? I don't think so because the block check happens before and if blocked, it will drop packet I think...


	Thread = xml:get_subtag_cdata(Packet, <<"thread">>),
	Type = xml:get_attr_s(<<"type">>, Attrs),

	case Type == <<"chat">> of
		true ->
			ejabberd_odbc:sql_query(Server,
                                [<<"UPDATE participants SET status='active' WHERE chat_id='">>,Thread,<<"' AND username='">>,ToUser,<<"'">>]);
		_ ->
			[]
	end,


	?INFO_MSG("\n\nGETTING HERE INSIDE ON_USER_SEND_PACKET?? JID: ~p. Type: ~p", [ Thread, Type ]),

	

        ?INFO_MSG("\nPacket Info:\n\n Name: ~p.\nAttrs: ~p.\nChildren: ~p.\nAll: ~p", [<<"message">>, Attrs, Children, Packet]),

Packet.
