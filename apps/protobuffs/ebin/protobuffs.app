{application, protobuffs,
 [
    {description, "Google protobuffs implementation for Erlang."},
    {vsn, "0.9"},
    {modules, [
		pokemon_pb,
		protobuffs,
		protobuffs_compile,
		protobuffs_parser
    ]},
    {registered, []},
    {applications, [
                    kernel,
                    stdlib,
                    sasl,
                    crypto
                   ]}
]}.
