%%%----------------------------------------------------------------------

%%% File    : mod_offline_post.erl
%%% Author  : Adam Duke <adam.v.duke@gmail.com>
%%% Purpose : Forward offline messages to an arbitrary url
%%% Created : 12 Feb 2012 by Adam Duke <adam.v.duke@gmail.com>
%%%
%%%
%%% Copyright (C) 2012   Adam Duke
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_offline_post).
-author('rgeorge@midnightweb.net').

-behaviour(gen_mod).

-export([start/2,
	 init/2,
	 stop/1,
	 send_notice/3,
	 send_notice_group/3,
	 send_post/5,
	 dispatch_post_by_type/7,
	 dispatch_confession_post/3,
	 get_active_group_participants/1]).

-define(PROCNAME, ?MODULE).
-define(SERVER, <<"versapp.co">>).
-define(PARTICIPANT_ACTIVE_STATUS, <<"active">>).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").

-import(mod_device_token, [get_token/1]).

start(Host, Opts) ->
    ?INFO_MSG("Starting mod_offline_post with Opts: ~p", [Opts]),
    AuthToken = gen_mod:get_module_opt(Host, ?MODULE, auth_token, fun(S) -> iolist_to_binary(S) end, list_to_binary("default")),
    PostUrl = gen_mod:get_module_opt(Host, ?MODULE, post_url, fun(S) -> iolist_to_binary(S) end, list_to_binary("default")),
    ?INFO_MSG("\n\nAuthToken: ~p - PostUrl: ~p\n", [AuthToken, PostUrl]),
    register(gen_mod:get_module_proc(Host, ?PROCNAME),
	     proc_lib:spawn(?MODULE, init, [Host, Opts])),
    ?INFO_MSG("REGISTERED", [] ),
    ok.

init(Host, _Opts) ->
    inets:start(),
    ssl:start(),
    ?INFO_MSG("Starting mod_offline_post. Host: ~p", [Host]),
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, send_notice, 10),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, send_notice_group, 11),
    ok.

stop(Host) ->
    ?INFO_MSG("Stopping mod_offline_post.", [] ),
    ejabberd_hooks:delete(offline_message_hook, Host,
			  ?MODULE, send_notice, 10),
    ok.

send_notice(From, To, Packet) ->
    ?INFO_MSG("\n\n\n\nsend_notice(): \n", []),
    
    Type = xml:get_tag_attr_s(list_to_binary("type"), Packet),
    Body = xml:get_path_s(Packet, [{elem, list_to_binary("body")}, cdata]),
    Thread = xml:get_path_s(Packet, [{elem, list_to_binary("thread")}, cdata]),

    ConnectionToken = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, auth_token, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
    PostUrl = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, post_url, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),

    ?INFO_MSG("DISPATCH_POST_BY_TYPE:\nType: ~p - Body: ~p\n", [Type, Body]),
    dispatch_post_by_type(Type, From, To, Body, PostUrl, ConnectionToken, [{"thread", Thread}]).

%% 'groupchat' messages do not activate offline_message_hook so I had to create this method hooked with 'user_send_packet'. This is a little hacky but needed to do.
send_notice_group(From, To, Packet)->
    
    Type = xml:get_tag_attr_s(list_to_binary("type"), Packet),
    Body = xml:get_path_s(Packet, [{elem, list_to_binary("body")}, cdata]),
    ConnectionToken = gen_mod:get_module_opt(From#jid.lserver, ?MODULE, auth_token, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
    PostUrl = gen_mod:get_module_opt(From#jid.lserver, ?MODULE, post_url, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),

    ?INFO_MSG("\n\n\nSEND_NOTICE_GROUP", []),

    %% This check is a little redundant but needs to be here otherwise the hook will activate twice when 'chat' messages are sent. 
    case Type of
	<<"groupchat">> ->
		dispatch_post_by_type(Type, From, To, Body, PostUrl, ConnectionToken, []);
	_->
		ok
    end,
ok.


%%% The following url encoding code is from the yaws project and retains it's original license.
%%% https://github.com/klacke/yaws/blob/master/LICENSE
%%% Copyright (c) 2006, Claes Wikstrom, klacke@hyber.org
%%% All rights reserved.
url_encode([H|T]) when is_list(H) ->
    [url_encode(H) | url_encode(T)];
url_encode([H|T]) ->
    if
        H >= $a, $z >= H ->
            [H|url_encode(T)];
        H >= $A, $Z >= H ->
            [H|url_encode(T)];
        H >= $0, $9 >= H ->
            [H|url_encode(T)];
        H == $_; H == $.; H == $-; H == $/; H == $: -> % FIXME: more..
            [H|url_encode(T)];
        true ->
            case integer_to_hex(H) of
                [X, Y] ->
                    [$%, X, Y | url_encode(T)];
                [X] ->
                    [$%, $0, X | url_encode(T)]
            end
     end;

url_encode([]) ->
    [].

integer_to_hex(I) ->
    case catch erlang:integer_to_list(I, 16) of
        {'EXIT', _} -> old_integer_to_hex(I);
        Int         -> Int
    end.

old_integer_to_hex(I) when I < 10 ->
    integer_to_list(I);
old_integer_to_hex(I) when I < 16 ->
    [I-10+$A];
old_integer_to_hex(I) when I >= 16 ->
    N = trunc(I/16),
    old_integer_to_hex(N) ++ old_integer_to_hex(I rem 16).

dispatch_post_by_type(<<"chat">>, From, To, Body, PostUrl, ConnectionToken, ExtraParams)->
	send_custom_post(From#jid.luser, To#jid.luser, Body, PostUrl, ConnectionToken, ExtraParams);

dispatch_post_by_type(<<"groupchat">>, From, To, Body, PostUrl, ConnectionToken, ExtraParams)->

	Participants = get_active_group_participants(To#jid.luser),

	FilteredParticipants = lists:delete(From#jid.luser, Participants),

	?INFO_MSG("\n\nGROUPCHAT DISPATCH: From: ~p, To: ~p\nParticipants: ~p", [From, To, FilteredParticipants]),

	lists:foreach( fun(Participant)->
		?INFO_MSG("\nSending to participant in group (offline): ~p", [Participant]),
		send_post(From#jid.luser, Participant, Body, PostUrl, ConnectionToken)
	end, FilteredParticipants),	
	

        ok;
dispatch_post_by_type( Type, From, To, Body, PostUrl, ConnectionToken, ExtraParams)->
	?INFO_MSG("I don't know how to dispatch this type of message: ~p", [Type]),
        ok.

dispatch_confession_post(To, Body, ConfessionId )->
        
        ?INFO_MSG("\nSending THOUGHT notification to ~p. Body: ~p", [To, Body]),

        ConnectionToken = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, auth_token, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
        PostUrl = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, post_url, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
        
        send_custom_post(<<"Versapp.Thoughts">>, To#jid.luser, Body, PostUrl, ConnectionToken, [{"confession_id", ConfessionId}]),
        ?INFO_MSG("\nThought Notification Sent", [ ]).


send_post(FromString, ToString, Body, PostUrl, ConnectionToken)->
	?INFO_MSG("Sending offline post to: ~p. JID: ~p, ConnectionToken: ~p, AccessToken: ~p, PostUrl: ~p, Body: ~p, From: ~p", [ToString, jlib:make_jid(ToString, ?SERVER, <<"">>), ConnectionToken, get_token(jlib:make_jid(ToString, ?SERVER, <<"">>)), PostUrl, Body, FromString]),
	Sep = "&",
        Post = [
          "token=", ConnectionToken, Sep,
          "to=", ToString, Sep,
          "from=", FromString, Sep,
          "body=", url_encode(binary_to_list(Body)), Sep,
          "access_token=", get_token(jlib:make_jid(ToString, ?SERVER, <<"">>))],
        
	httpc:request(post, {binary_to_list(PostUrl), [], "application/x-www-form-urlencoded", list_to_binary(Post)}, [], []),
        
	ok.

% ExtraParamList follows: [{key,val},{key,val},...]
send_custom_post(FromString, ToString, Body, PostUrl, ConnectionToken, ExtraParamList) ->

	%% Takes a list of tuples representing key,val pairs and transforms it into a post param string.
	ExtraPostParamString = lists:flatten(string:join(lists:map(fun(El)-> {Key, Val} = El,  [Key, "=", Val]  end, ExtraParamList), "&")),

	Sep = "&",
        Post = [
          "token=", ConnectionToken, Sep,
          "to=", ToString, Sep,
          "from=", FromString, Sep,
          "body=", url_encode(binary_to_list(Body)), Sep,
          "access_token=", get_token(jlib:make_jid(ToString, ?SERVER, <<"">>))] ++ Sep ++ ExtraPostParamString,

	?INFO_MSG( "\nPost associated with notification. ", [ list_to_binary(Post) ] ),

        httpc:request(post, {binary_to_list(PostUrl), [], "application/x-www-form-urlencoded", list_to_binary(Post)}, [], []),

ok.


get_active_group_participants(ChatId)->
	{_,_, Result} = ejabberd_odbc:sql_query(?SERVER,
                                [<<"SELECT p.username FROM participants p WHERE chat_id='">>,ChatId,<<"' AND status='active' AND username NOT IN (SELECT username FROM session)">>]),

	lists:flatten(Result).
