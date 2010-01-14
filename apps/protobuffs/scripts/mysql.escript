#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sasl errlog_type error -boot start_sasl -noshell

%% ====================================================
%% Generate proto message from mysql table definition
%% $> escript mysql.escript host port user pwd db table
%% ====================================================

main([Host, Port0, User, Password, Database, Table]) ->
	ok = crypto:start(),
	Greeting_Fun =
		fun(_M,_L,_E,_F) -> 
			%{Format,Args} = F(),
			%io:format("[~p:~b] ~p~n", [M,L,E]),
			%io:format(Format ++ "~n", Args),
			ok
		end,
		
	Port = list_to_integer(Port0),
	{ok, Pid} = mysql_conn:start(Host, Port, User, Password, Database, Greeting_Fun, undefined, undefined),

	[C|Rest] = Table,
	MessageName = [string:to_upper(C)|Rest],
	case mysql_conn:fetch(Pid, list_to_binary(lists:flatten(io_lib:format("DESC ~s", [Table]))), self()) of 
        {data, MySQLRes} ->
            Rows = mysql:get_result_rows(MySQLRes),
			io:format("message ~s {~n", [MessageName]),
			print_row(Rows, 1),
			io:format("}~n");
		[] -> 
			[];
		Err -> 
			error_logger:error_msg("~p~n", [Err])
    end,

	ok;
	
main(_Args) ->
	io:format("usage: escript mysql.escript host port user password database table~n"),
	io:format("args: ~p~n", [_Args]).
	
print_row([], _) -> ok;
print_row([[Field, Type, Null, _Key, Default, _Extra]|Tail], Number) ->
	Null1 = null(Null),
	Type1 = type(binary_to_list(Type)),
	Default1 = default(Type1, Default),
	io:format("    ~s ~s ~s = ~w~s;~n", [Null1, Type1, Field, Number, Default1]),

	print_row(Tail, Number+1).
		
null(<<"YES">>) -> "optional";
null(<<"NO">>) -> "required".

type("tinyint" ++ _) -> "int32";
type("bigint" ++ _) -> "int64";
type("serial" ++ _) -> "int64";
type("mediumint" ++ _) -> "int32";
type("int" ++ _) -> "int32";
type("datetime" ++ _) -> "int32";
type("float" ++ _) -> "float";
type("double" ++ _) -> "double";
type("decimal" ++ _) -> "double";
type("varchar" ++ _) -> "string";
type("char" ++ _) -> "string";
type("text" ++ _) -> "string";
type(Type) -> Type.


default(_, <<"NULL">>) -> "";
default(_, undefined) -> "";
default("int" ++ _, Default) -> lists:flatten(io_lib:format(" [default = ~w]", [list_to_integer(binary_to_list(Default))]));
default(_, Default) -> lists:flatten(io_lib:format(" [default = ~p]", [binary_to_list(Default)])).
