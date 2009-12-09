-record(wm_log_data, 
	{resource_module :: atom(),
	 start_time :: tuple(),
	 method :: atom(),
	 headers,
	 peer, 
	 path :: string(),
	 version,
	 response_code,
	 response_length,
	 end_time :: tuple(),
	 finish_time :: tuple()}).

