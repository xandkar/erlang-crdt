%%%----------------------------------------------------------------------------
%%% Observed-Remove Set (OR-Set)
%%%----------------------------------------------------------------------------
-module(crdt_set_or).

-export_type(
    [ t/1
    ]).

-export(
    [ empty/0
    , is_member/2
    , add/2
    , remove/2
    , merge/2
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
