% Assignment 1 - Ted Johnson 19335618

% === Definitions ===
% Frontier = A list of Nodes
% Node = [Path, Cost]
% KeyBase = A list of States
% Path = A lists of States
% State = A list

% Node node, Node childOfNode, KeyBase kb
% childOfNode is a valid arc from node found in kb.
arc([[[H|T]|ParentStates],Cost], [[ChildNode,[H|T]|ParentStates],ChildCost], KB) :-
	member([H|ChildState], KB), append(ChildState, T, ChildNode),
	length(ChildState, Length), ChildCost is 1 + Length/(Length+1) + Cost.

% State goalState
% goalState is an accepted state.
goal([]).

% State state, Number heuristic
% heuristic is h(state).
heuristic(State, Heuristic) :- length(State, Heuristic).

% Node a, Node b
% Node a has a lower or equal search function than node b. That is, f(a) <= f(b).
lessthan([[StateA|_], CostA], [[StateB|_], CostB]) :-
	heuristic(StateA, HeuristicA), heuristic(StateB, HeuristicB),
	FA is CostA + HeuristicA, FB is CostB + HeuristicB,
	FA =< FB.

% Frontier newNodes, Frontier oldFrontier, Frontier newFrontier
% newFrontier is the inclusion of all nodes in newNodes into oldFrontier in accordance
% with the A* algorithm. That is, f(n) < f(n+1) for every node n in newFrontier.
add2frontier([], OldFrontier, OldFrontier).
add2frontier([NewNode|NextNewNodes], OldNodes, NewFrontier) :-
	insert(NewNode, OldNodes, SortedNodes),
	add2frontier(NextNewNodes, SortedNodes, NewFrontier).

% Node newNode, Frontier sortedNodes, Frontier newSortedNodes
% newSortedNodes is the inclusion of newNode into sortedNodes such that newSortedNodes
% is sorted in accending order of f(n) using insertion sort.
insert(NewNode, [OldNode|NextOldNodes], [OldNode|SortedNodes]) :-
	lessthan(OldNode, NewNode), !,
	insert(NewNode, NextOldNodes, SortedNodes).
insert(NewNode, OldNodes, [NewNode|OldNodes]).

% Frontier frontier, Path pathToGoal, Cost pathToGoalCost, KeyBase kb
% pathToGoal is a path of states from one of the nodes present in frontier to the
% goal state with a total cost of pathToGoalCost according to arcs found in kb.
search([[[State|ParentStates],Cost]|_], [State|ParentStates], Cost, _) :- goal(State).
search([Node|NextNodes], FoundPath, FoundCost, KB) :-
	findall(X, arc(Node, X, KB), NewNodes),
	add2frontier(NewNodes, NextNodes, NewFrontier),
	%write("Frontier"), nl, printFrontier(NewFrontier), nl,
	search(NewFrontier, FoundPath, FoundCost, KB).

% State initialState, Path pathToGoal, Cost pathToGoalCost, KeyBase kb
% pathToGoal is a path of states from initialState to the goal state with a total
% cost of pathToGoalCost according to arcs found in kb.
astar(InitialState, Path, Cost, KB) :-
	search([[[InitialState], 0]], ReversePath, Cost, KB),
	reverse(ReversePath, Path).

% Frontier frontier
% Print the contents of a frontier for debugging purposes.
%printFrontier([]).
%printFrontier([[[State|_],Cost]|NextNode]) :-
%	heuristic(State, Heuristic),
%	F is Cost + Heuristic,
%	write(State), write(": "), write(Cost+Heuristic=F), nl,
%	printFrontier(NextNode).

/** <examples>
?- astar([q], Path, Cost, [[q,a],[q,b,c],[a],[b],[c]]).
?- astar([q], Path, Cost, [[q,a],[q,b,c],[a,d,e],[a,c,e,f],[b,c],[c,e,f],[e],[f,e]]).
?- astar([q], Path, Cost, [[q,q],[q,a],[a]]).
?- astar([q], Path, Cost, [[q,a,b,c],[q,d],[a],[b],[c],[d,e],[e,f],[e]]).
?- astar(Initial, Path, Cost, [[q,a],[q,b,c],[a,d,e],[a,c,e,f],[b,c],[c,e,f],[e],[f,e]]).
*/
