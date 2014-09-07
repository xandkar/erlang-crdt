-module(crdt_set_2p_SUITE).

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


-define(set_module , crdt_set_2p).
-define(GROUP      , ?set_module).


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
    ValB = <<"bar">>,  % Concurrently added in B and C, removed from B

    % Initialize original
    SetA1 = ?set_module:empty(),
    false = ?set_module:is_member(SetA1, ValA),
    SetA2 = ?set_module:add      (SetA1, ValA),
    SetA2 = ?set_module:add      (SetA2, ValA),  % Should have no effect
    true  = ?set_module:is_member(SetA2, ValA),

    % Replicate
    SetB1 = SetA2,
    SetC1 = SetA2,

    % Assert replicated value is in both replicas
    true  = ?set_module:is_member(SetB1, ValA),
    true  = ?set_module:is_member(SetC1, ValA),

    % Concurrently add same new value in both replicas
    SetB2 = ?set_module:add      (SetB1, ValB),
    SetC2 = ?set_module:add      (SetC1, ValB),
    true  = ?set_module:is_member(SetB2, ValB),
    true  = ?set_module:is_member(SetC2, ValB),

    % Remove new value from one (should take effect in all replicas)
    SetB3 = ?set_module:remove   (SetB2, ValB),
    false = ?set_module:is_member(SetB3, ValB),

    % Merge
    SetD = lists:foldl(fun ?set_module:merge/2, SetA2, [SetB3, SetC2]),

    % Assert replicated value is there
    true = ?set_module:is_member(SetD , ValA),

    % Assert removed is gone
    false = ?set_module:is_member(SetD , ValB).

t_serialization(_Cfg) ->
    ValToBin = fun (Val) -> Val end,
    ValOfBin = fun (Bin) -> {ok, Bin} end,
    ValA = <<"foo">>,
    ValB = <<"bar">>,
    Set0       = ?set_module:empty(),
    Set1       = ?set_module:add      (Set0   , ValA),
    Set2       = ?set_module:add      (Set1   , ValB),
    Set3       = ?set_module:remove   (Set2   , ValB),
    SetBin     = ?set_module:to_bin   (Set3   , ValToBin),
    {ok, Set3} = ?set_module:of_bin   (SetBin , ValOfBin),
    true       = ?set_module:is_member(Set3   , ValA),
    false      = ?set_module:is_member(Set3   , ValB),

    BadBin1 = <<"{\"foo\": []}">>,
    {error, {parsing_error, _}} = ?set_module:of_bin(BadBin1, ValOfBin),

    ValOfBin2 = fun (<<"good">> = V) -> {ok, V}; (_) -> {error, bad_value} end,
    BadBin2 = <<"{\"members\": [\"good\"], \"tombstones\": [\"bad\"]}">>,
    {error, {parsing_error, _}} = ?set_module:of_bin(BadBin2, ValOfBin2).
