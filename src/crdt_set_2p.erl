%%%----------------------------------------------------------------------------
%%% Two-Phase Set (2P-Set):
%%% > an element may be added and removed, but never added again thereafter.
%%%
%%% Marc Shapiro, Nuno Preguiça, Carlos Baquero, and Marek Zawirski.
%%% "A comprehensive study of Convergent and Commutative Replicated Data Types"
%%% Rapport de recherche 7506, Institut Nat. de la Recherche en Informatique et
%%% Automatique (INRIA), Rocquencourt, France, January 2011.
%%%----------------------------------------------------------------------------
-module(crdt_set_2p).

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
    , to_props/2
    , of_props/2
    ]).


-record(t,
    { members
    , tombstones
    }).

-opaque t(A) ::
    #t
    { members    :: [A]
    , tombstones :: [A]
    }.

-type parsing_error() ::
    {parsing_error, term()}.  % TODO: Complete the error specification


-define(FIELD_MEMBERS    , <<"members">>).
-define(FIELD_TOMBSTONES , <<"tombstones">>).


-spec empty() ->
    t(_A).
empty() ->
    #t
    { members    = []
    , tombstones = []
    }.

-spec is_member(t(A), A) ->
    boolean().
is_member(#t{members=Members}, X) ->
    lists:member(X, Members).

-spec add(t(A), A) ->
    t(A).
add(#t{members=Xs}=T, X) ->
    % TODO: Maybe return error if already exists?
    case is_member(T, X)
    of  true  -> T
    ;   false -> T#t{members=[X | Xs]}
    end.

-spec remove(t(A), A) ->
    t(A).
remove(#t{members=Members1, tombstones=Tombstones1}=T, X) ->
    {Tombstones2, Members2} = lists:partition(fun(M) -> M =:= X end, Members1),
    % TODO: Maybe return error if not found?
    T#t
    { members    = Members2
    , tombstones = Tombstones1 ++ Tombstones2
    }.

-spec merge(t(A), t(A)) ->
    t(A).
merge(TA, TB) ->
    #t{members=MembersA, tombstones=TombstonesA} = TA,
    #t{members=MembersB, tombstones=TombstonesB} = TB,
    Tombstones = lists:usort(TombstonesA ++ TombstonesB),
    Members1   = lists:usort(MembersA    ++ MembersB),
    Members2   = Members1 -- Tombstones,
    #t
    { members    = Members2
    , tombstones = Tombstones
    }.

-spec to_bin(t(A), fun((A) -> binary())) ->
    binary().
to_bin(#t{}=T, ValueToBin) ->
    Props = to_props(T, ValueToBin),
    jsx:encode(Props).

-spec of_bin(binary(), fun( (binary()) -> hope_result:t(A, _B) )) ->
    hope_result:t(t(A), parsing_error()).
of_bin(Bin, BinToVal) ->
    Decode = hope_result:lift_exn(fun jsx:decode/1),
    Construct = fun (Props) -> of_props(Props, BinToVal) end,
    Steps =
        [ Decode
        , Construct
        ],
    case hope_result:pipe(Steps, Bin)
    of  {ok    , _}=Ok -> Ok
    ;   {error , E}    -> {error, {parsing_error, E}}
    end.

-spec to_props(t(A), fun((A) -> binary())) ->
    binary().
to_props(#t{members=Members, tombstones=Tombstones}, ValueToBin) ->
    [ {?FIELD_MEMBERS    , lists:map(ValueToBin, Members)}
    , {?FIELD_TOMBSTONES , lists:map(ValueToBin, Tombstones)}
    ].

-spec of_props(binary(), fun((binary()) -> hope_result:t(A, _B))) ->
    hope_result:t(t(A), parsing_error()).
of_props(Props, BinToVal) ->
    Validate =
        fun (ok) ->
            FieldsRequired =
                [ ?FIELD_MEMBERS
                , ?FIELD_TOMBSTONES
                ],
            hope_kv_list:validate_unique_presence(Props, FieldsRequired)
        end,
    Construct =
        fun (ok) ->
            {some, MemberBins}    = hope_kv_list:get(Props, ?FIELD_MEMBERS),
            {some, TombstoneBins} = hope_kv_list:get(Props, ?FIELD_TOMBSTONES),
            ParseMembers = fun (ok) -> bins_to_vals(MemberBins, BinToVal) end,
            ParseTombstones =
                fun (Members) ->
                    case bins_to_vals(TombstoneBins, BinToVal)
                    of  {ok, Tombstones} -> {ok, {Members, Tombstones}}
                    ;   {error, _}=Error -> Error
                    end
                end,
            Construct =
                fun ({Members, Tombstones}) ->
                    T = #t
                        { members    = Members
                        , tombstones = Tombstones
                        },
                    {ok, T}
                end,
            Steps =
                [ ParseMembers
                , ParseTombstones
                , Construct
                ],
            hope_result:pipe(Steps, ok)
        end,
    Steps =
        [ Validate
        , Construct
        ],
    case hope_result:pipe(Steps, ok)
    of  {ok   , _}=Ok -> Ok
    ;   {error, _}=Er -> Er
    end.


%% ============================================================================
%% Helpers
%% ============================================================================

-spec bins_to_vals([binary()], fun((binary()) -> hope_result:t(A, B))) ->
    hope_result:t([A], B).
bins_to_vals(Bins, BinToVal) ->
    Steps = [bin_to_bin_parser_accumulator(B, BinToVal) || B <- Bins],
    hope_result:pipe(Steps, []).

-spec bin_to_bin_parser_accumulator(binary(), BinToVal) ->
    hope_result:t(ParseAcc, B)
    when BinToVal :: fun(( binary() ) -> hope_result:t( A , B))
       , ParseAcc :: fun(([binary()]) -> hope_result:t([A], B))
       .
bin_to_bin_parser_accumulator(Bin, BinToVal) ->
    fun (Acc) ->
        case BinToVal(Bin)
        of  {ok, Val}    -> {ok, [Val | Acc]}
        ;   {error, _}=E -> E
        end
    end.
