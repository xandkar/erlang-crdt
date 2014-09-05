-module(crdt_id_unique).

-export_type(
    [ t/0
    ]).

-export(
    [ new/0
    ]).

-type t() ::
    binary().


-spec new() ->
    binary().
new() ->
    list_to_binary(uuid:uuid_to_string(uuid:get_v4())).
