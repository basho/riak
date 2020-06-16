-module(riak_schema_test).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all, nowarn_export_all]).

-define(riak_schema, "priv/riak.schema").
-define(eleveldb_schema, "_build/test/lib/eleveldb/priv/eleveldb.schema").

basic_schema_test() ->
    Config = cuttlefish_unit:generate_templated_config(
               [?riak_schema], [], context(), predefined_schema()),

    cuttlefish_unit:assert_config(Config, "lager.handlers",
                                  [
                                   {lager_file_backend, [{file, "./log/console.log"},
                                                         {level, info},
                                                         {size, 10485760},
                                                         {date, "$D0"},
                                                         {count, 5}]},
                                   {lager_file_backend, [{file, "./log/error.log"},
                                                         {level, error},
                                                         {size, 10485760},
                                                         {date, "$D0"},
                                                         {count, 5}]}
                                  ]),
    cuttlefish_unit:assert_config(Config, "sasl.sasl_error_logger", false),
    cuttlefish_unit:assert_config(Config, "lager.crash_log", "./log/crash.log"),
    cuttlefish_unit:assert_config(Config, "lager.crash_log_msg_size", 64 * 1024),
    cuttlefish_unit:assert_config(Config, "lager.crash_log_size", 10 * 1024 * 1024),
    cuttlefish_unit:assert_config(Config, "lager.crash_log_date", "$D0"),
    cuttlefish_unit:assert_config(Config, "lager.crash_log_count", 5),
    cuttlefish_unit:assert_config(Config, "lager.error_logger_redirect", true),
    cuttlefish_unit:assert_config(Config, "lager.error_logger_hwm", 100),
    cuttlefish_unit:assert_config(Config, "vm_args.-setcookie", "riak"),
    ok.

override_schema_test() ->
    Conf = [{["log", "console"], both},
            {["log", "console", "level"], debug},
            {["log", "console", "file"], "/var/log/foo.log"},
            {["log", "error", "file"], "/var/log/errors.log"},
            {["log", "syslog"], on},
            {["sasl"], on},
            {["log", "crash", "file"], "/var/log/crashy.log"},
            {["log", "crash", "maximum_message_size"], "1KB"},
            {["log", "crash", "size"], "2KB"},
            {["log", "crash", "rotation"], "$W0"},
            {["log", "crash", "rotation", "keep"], current},
            {["log", "error", "redirect"], off},
            {["log", "error", "messages_per_second"], 50},
            {["distributed_cookie"], "tyktorp"}],

    Config = cuttlefish_unit:generate_templated_config(
               [?riak_schema], Conf, context(), predefined_schema()),

    cuttlefish_unit:assert_config(Config, "lager.handlers",
                                  [{lager_syslog_backend, ["riak", daemon, info]},
                                   {lager_console_backend, debug},
                                   {lager_file_backend, [{file, "/var/log/foo.log"},
                                                         {level, debug},
                                                         {size, 10485760},
                                                         {date, "$D0"},
                                                         {count, 5}]},
                                   {lager_file_backend, [{file, "/var/log/errors.log"},
                                                         {level, error},
                                                         {size, 10485760},
                                                         {date, "$D0"},
                                                         {count, 5}]}
                                  ]),
    cuttlefish_unit:assert_config(Config, "sasl.sasl_error_logger", true),
    cuttlefish_unit:assert_config(Config, "lager.crash_log", "/var/log/crashy.log"),
    cuttlefish_unit:assert_config(Config, "lager.crash_log_msg_size", 1024),
    cuttlefish_unit:assert_config(Config, "lager.crash_log_size", 2048),
    cuttlefish_unit:assert_config(Config, "lager.crash_log_date", "$W0"),
    cuttlefish_unit:assert_config(Config, "lager.crash_log_count", 0),
    cuttlefish_unit:assert_config(Config, "lager.error_logger_redirect", false),
    cuttlefish_unit:assert_config(Config, "lager.error_logger_hwm", 50),
    cuttlefish_unit:assert_config(Config, "vm_args.-setcookie", "tyktorp"),
    ok.

custom_syslog_test() ->
    Conf = [{["log", "syslog"], on},
            {["log","syslog","ident"], "VeryCool"},
            {["log","syslog","facility"], local4},
            {["log","syslog","level"], warning}],
    Config = cuttlefish_unit:generate_templated_config(
               [?riak_schema], Conf, context(), predefined_schema()),
    cuttlefish_unit:assert_config(Config, "lager.handlers",
                                  [{lager_syslog_backend, ["VeryCool", local4, warning]},
                                   {lager_file_backend, [{file, "./log/console.log"},
                                                         {level, info},
                                                         {size, 10485760},
                                                         {date, "$D0"},
                                                         {count, 5}]},
                                   {lager_file_backend, [{file, "./log/error.log"},
                                                         {level, error},
                                                         {size, 10485760},
                                                         {date, "$D0"},
                                                         {count, 5}]}
                                  ]),
    ok.

crash_log_test() ->
    Conf = [{["log", "crash"], off}],
    Config = cuttlefish_unit:generate_templated_config(
               [?riak_schema], Conf, context(), predefined_schema()),
    cuttlefish_unit:assert_config(Config, "lager.crash_log", undefined),
    ok.

devrel_test() ->
    RelConfig = cuttlefish_unit:generate_templated_config(
                  [?riak_schema,
                  ?eleveldb_schema],
                  [], context(), predefined_schema()),

    cuttlefish_unit:assert_config(RelConfig, "eleveldb.limited_developer_mem", false),
    cuttlefish_unit:assert_not_configured(RelConfig, "vm_args.-shutdown_time"),

    DevRelConfig = cuttlefish_unit:generate_templated_config(
                     [?riak_schema,
                      ?eleveldb_schema], [],
                     lists:keyreplace(devrel, 1, context(), {devrel, true}), predefined_schema()),

    cuttlefish_unit:assert_config(DevRelConfig, "eleveldb.limited_developer_mem", true),
    cuttlefish_unit:assert_config(DevRelConfig, "vm_args.-shutdown_time", 10000),
    ok.

context() ->
    [{console_log_default, file},
     {platform_log_dir, "./log"},
     {devrel, false}].

%% This predefined schema covers riak's dependency on
%% platform_log_dir
predefined_schema() ->
    Mappings = [cuttlefish_mapping:parse({mapping,
                                        "platform_log_dir",
                                        "riak_core.platform_log_dir", [
                                            {default, "./log"},
                                            {datatype, directory}
                                       ]}),
                cuttlefish_mapping:parse({mapping,
                                        "platform_data_dir",
                                        "riak_core.platform_data_dir", [
                                            {default, "./data"},
                                            {datatype, directory}
                                       ]})
               ],

    {[], Mappings, []}.
