%%%----------------------------------------------------------------------------
%%% Observed-Remove Set (OR-Set)
%%%----------------------------------------------------------------------------
-module(crdt_set_or).

-export_type(
    [ t/1
    , parsing_error/0
    ]).

-export(
    [ empty/0
    , is_member/2
    , add/2
    , remove/2
    , merge/2
    , to_bin/2
    , of_bin/2
    ]).


-record(member,
    { value
    , id :: crdt_id_unique:t()
    }).

-type member(A) ::
    #member{value :: A}.

-record(t,
    { members
    , tombstones
    }).

-opaque t(A) ::
    #t
    { members    :: [member(A)]
    , tombstones :: [member(A)]
    }.

-type parsing_error() ::
    {parsing_error, term()}.  % TODO: Complete the error specification


-define(FIELD_MEMBERS    , <<"members">>).
-define(FIELD_TOMBSTONES , <<"tombstones">>).
-define(FIELD_ID         , <<"id">>).
-define(FIELD_VALUE      , <<"value">>).


-spec empty() ->
    t(_A).
empty() ->
    #t
    { members    = []
    , tombstones = []
    }.

-spec is_member(t(A), A) ->
    boolean().
is_member(#t{members=Members}, MemberValue) ->
    % TODO: More efficinet implementation. Only need to find the 1st member.
    case [V || #member{value=V} <- Members, V =:= MemberValue]
    of  [_|_] -> true
    ;   []    -> false
    end.

-spec add(t(A), A) ->
    t(A).
add(#t{members=Members}=T, Value) ->
    % TODO: Maybe return error if already exists?
    case is_member(T, Value)
    of  true ->
            T
    ;   false ->
            Member = #member
                { value = Value
                , id    = crdt_id_unique:new()
                },
            T#t{members=[Member | Members]}
    end.

-spec remove(t(A), A) ->
    t(A).
remove(#t{members=M1, tombstones=TombsA}=T, Val) ->
    {TombsB, M2} = lists:partition(fun(#member{value=V}) -> V =:= Val end, M1),
    T#t
    { members    = M2
    , tombstones = TombsA ++ TombsB
    }.

-spec merge(t(A), t(A)) ->
    t(A).
merge(TA, TB) ->
    #t{members=MemsA, tombstones=TombsA} = TA,
    #t{members=MemsB, tombstones=TombsB} = TB,
    Tombs = lists:usort(TombsA ++ TombsB),
    Mems1 = lists:usort(MemsA  ++ MemsB),
    Mems2 = Mems1 -- Tombs,
    #t
    { members    = Mems2
    , tombstones = Tombs
    }.

-spec to_bin(t(A), fun((A) -> binary())) ->
    binary().
to_bin(#t{members=Mems, tombstones=Tombs}, ValueToBin) ->
    Props =
        [ {?FIELD_MEMBERS    , [member_to_props(M, ValueToBin) || M <- Mems]}
        , {?FIELD_TOMBSTONES , [member_to_props(T, ValueToBin) || T <- Tombs]}
        ],
    jsx:encode(Props).

-spec of_bin(binary(), fun( (binary()) -> A )) ->
    hope_result:t(t(A), parsing_error()).
of_bin(Bin, BinToVal) ->
    % TODO: Error handling
    Props = jsx:decode(Bin),
    {some, MemsProps}  = hope_kv_list:get(Props, ?FIELD_MEMBERS),
    {some, TombsProps} = hope_kv_list:get(Props, ?FIELD_TOMBSTONES),
    MemOfProps =
        fun (P) ->
            {ok, Member} = member_of_props(P, BinToVal),
            Member
        end,
    T = #t
    { members    = lists:map(MemOfProps, MemsProps)
    , tombstones = lists:map(MemOfProps, TombsProps)
    },
    {ok, T}.

-spec member_to_props(member(A), fun( (A) -> binary() )) ->
    [{binary(), binary()}].
member_to_props(#member{value=V, id = <<ID/binary>>}, ValueToBin) ->
    [ {?FIELD_ID    , ID}
    , {?FIELD_VALUE , ValueToBin(V)}
    ].

-spec member_of_props(binary(), fun( (binary()) -> hope_result:t(A, B) )) ->
    hope_result:t(t(A), Error)
    when Error :: {fields_missing, [binary()]}
                | {value_parsing_failure, B}
       .
member_of_props(Props, BinToVal) ->
    % TODO: Error handling
    {some, ValBin} = hope_kv_list:get(Props, ?FIELD_VALUE),
    {some, ID}     = hope_kv_list:get(Props, ?FIELD_ID),
    {ok, Val} = BinToVal(ValBin),
    Member = #member
    { value = Val
    , id    = ID
    },
    {ok, Member}.
