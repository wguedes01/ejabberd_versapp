%%%-------------------------------------------
%% Custom Namespaces
%%%-------------------------------------------
-define(NS_CONFESSION, <<"who:iq:confession">>).

%%%-------------------------------------------
%% Custom XML-formatted Errors
%%%-------------------------------------------
-define(VERSAPP_INTERNAL_SERVER_ERROR(Ns, ErrorCode, Description), 
	#xmlel{name = "custom", attrs = [{<<"code">>, ErrorCode }], children = [{xmlcdata, Description}]}).

-define(CONFESSION_CREATION_ERROR,
	?VERSAPP_INTERNAL_SERVER_ERROR(?NS_CONFESSION, <<"101">>, <<"FAILED">>)).

-record(confession, {
			id :: 'undefined' | non_neg_integer(),
			username :: binary(),
			body :: binary(),
			image_url :: binary(),
			created_timestamp :: 'undefined' | any()
			}).


