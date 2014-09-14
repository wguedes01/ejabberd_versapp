
-record(confession, {
			id :: 'undefined' | non_neg_integer(),
			username :: binary(),
			body :: binary(),
			image_url :: binary(),
			created_timestamp :: 'undefined' | any()
			}).
