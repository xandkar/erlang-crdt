-module(crdt_set_or_SUITE).

%% Callbacks
-export(
    [ all/0
    , groups/0
    ]).

%% Test cases
-export(
    [ t_replicate_crud_merge/1
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