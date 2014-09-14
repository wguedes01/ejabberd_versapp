-module(custom_odbc_queries).

-author("Guilherme Guedes").

-include("ejabberd.hrl").
-include("logger.hrl").
-include("custom_records.hrl").

-export([insert_confession/2]).

%%%------------------------
%%% Confession Queries
%%%------------------------

%% Database Info
-define(CONFESSIONS_TABLE, <<"confessions">>).
-define(CONFESSIONS_TABLE_COLUMN_CONFESSION_ID, <<"confession_id">>).
-define(CONFESSIONS_TABLE_COLUMN_USERNAME, <<"jid">>).
-define(CONFESSIONS_TABLE_COLUMN_BODY, <<"body">>).
-define(CONFESSIONS_TABLE_COLUMN_IMAGE_URL, <<"image_url">>).
-define(CONFESSIONS_TABLE_COLUMN_CREATED_TIMESTAMP, <<"created_timestamp">>).

%% Create Confession
%% Return: confession_id.
-type insert_confession_result() :: #confession{} | error.

-spec insert_confession(binary(), #confession{}) -> insert_confession_result().

insert_confession(Server, #confession{username=Username, body=Body, image_url=ImageUrl} = Confession)->

	Res = ejabberd_odbc:sql_query(Server,
                                [
					<<"INSERT INTO ">>,?CONFESSIONS_TABLE,<<" (
					">>,?CONFESSIONS_TABLE_COLUMN_USERNAME,<<",
					">>,?CONFESSIONS_TABLE_COLUMN_BODY,<<",
					">>,?CONFESSIONS_TABLE_COLUMN_IMAGE_URL,<<"
					) VALUES (
					'">>,Username,<<"',
					'">>,Body,<<"',
					 '">>,ImageUrl,<<"'
					)">>
				]),

	case Res of
		{updated, 1} ->
			IdAndTimeRes = ejabberd_odbc:sql_query(Server, 
				[
					<<"SELECT ">>
					,?CONFESSIONS_TABLE_COLUMN_CONFESSION_ID,<<", ">>
					,<<"UNIX_TIMESTAMP(">>,?CONFESSIONS_TABLE_COLUMN_CREATED_TIMESTAMP,<<") AS ">>,?CONFESSIONS_TABLE_COLUMN_CREATED_TIMESTAMP, <<" ">>
					,<<"FROM ">>
					,?CONFESSIONS_TABLE,<<" ">>
					,<<"WHERE ">>
					,?CONFESSIONS_TABLE_COLUMN_USERNAME,<<"='">>,Username,<<"' ">>
					,<<"ORDER BY ">>
					,?CONFESSIONS_TABLE_COLUMN_CREATED_TIMESTAMP,<<" DESC LIMIT 1">>
				]),

			case IdAndTimeRes of
				{
					selected, 
					[
						?CONFESSIONS_TABLE_COLUMN_CONFESSION_ID, 
						?CONFESSIONS_TABLE_COLUMN_CREATED_TIMESTAMP
					],
					Result
				} ->
					[[IdBinary,CreatedTimeBinary]] = Result,
					Confession#confession{id = IdBinary, created_timestamp = CreatedTimeBinary};
				
				_->
					?INFO_MSG("Failed to retrieve confession after insert success: ~p", [IdAndTimeRes]),
					error				
			end;
		_-> 
			?INFO_MSG("An error occurred inserting a confession on db: ~p", [Res]),
			error
	end.
