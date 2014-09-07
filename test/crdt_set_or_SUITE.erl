-module(crdt_set_or_SUITE).

%% Callbacks
-export(
    [ all/0
    , groups/0
    ]).

%% Test cases
-export(
    [ t_replicate_crud_merge/1
    , t_serialization/1
    ]).


-define(GROUP , crdt_set_or).


%% ============================================================================
%% Common Test callbacks
%% ============================================================================

all() ->
    [{group, ?GROUP}].

groups() ->
    Tests =
        [ t_replicate_crud_merge
        , t_serialization
        ],
    Properties = [],
    [{?GROUP, Properties, Tests}].


%% =============================================================================
%%  Test cases
%% =============================================================================

t_replicate_crud_merge(_Cfg) ->
    ValA = <<"foo">>,  % Replicated from A into B and C
    ValB = <<"bar">>,  % Concurrently added in B and C
    ValC = <<"qux">>,  % Unique to C, unobserved from B

    % Initialize original
    SetA1 = crdt_set_or:empty(),
    false = crdt_set_or:is_member(SetA1, ValA),
    SetA2 = crdt_set_or:add      (SetA1, ValA),
    SetA2 = crdt_set_or:add      (SetA2, ValA),  % Should have no effect
    true  = crdt_set_or:is_member(SetA2, ValA),

    % Replicate
    SetB1 = SetA2,
    SetC1 = SetA2,

    % Assert replicated value in both replicas
    true  = crdt_set_or:is_member(SetB1, ValA),
    true  = crdt_set_or:is_member(SetC1, ValA),

    % Concurrently add
    SetB2 = crdt_set_or:add      (SetB1, ValB),
    SetC2 = crdt_set_or:add      (SetC1, ValB),

    % Remove unobserved
    SetB3 = crdt_set_or:remove   (SetB2, ValC),
    % Add unique
    SetC3 = crdt_set_or:add      (SetC2, ValC),

    % Remove replicated from one (should take effect in all replicas)
    SetB4 = crdt_set_or:remove   (SetB3, ValA),
    false = crdt_set_or:is_member(SetB4, ValA),

    % Remove concurrently added from one (should take effect in only one)
    SetB5 = crdt_set_or:remove   (SetB4, ValB),
    false = crdt_set_or:is_member(SetB5, ValB),

    % Merge
    SetD = lists:foldl(fun crdt_set_or:merge/2, SetA2, [SetB4, SetC3]),

    % Assert removed replicated is gone
    false = crdt_set_or:is_member(SetD , ValA),

    % Assert one of concurrently added is still there
    true = crdt_set_or:is_member(SetD , ValB),

    % Assert removed unobserved still there
    true  = crdt_set_or:is_member(SetD , ValB).

t_serialization(_Cfg) ->
    ValToBin = fun (Val) -> Val end,
    ValOfBin = fun (Bin) -> {ok, Bin} end,
    ValA = <<"foo">>,
    ValB = <<"bar">>,
    Set0       = crdt_set_or:empty(),
    Set1       = crdt_set_or:add      (Set0   , ValA),
    Set2       = crdt_set_or:add      (Set1   , ValB),
    Set3       = crdt_set_or:remove   (Set2   , ValB),
    SetBin     = crdt_set_or:to_bin   (Set3   , ValToBin),
    {ok, Set3} = crdt_set_or:of_bin   (SetBin , ValOfBin),
    true       = crdt_set_or:is_member(Set3   , ValA),
    false      = crdt_set_or:is_member(Set3   , ValB),

    BadBin = <<"{\"foo\": []}">>,
    {error, {parsing_error, _}} = crdt_set_or:of_bin(BadBin , ValOfBin).
