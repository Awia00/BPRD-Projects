module Nfa
open Absyn

let symValue sym : int = 
    match sym with
    | SEps -> -1
    | SChar c -> int c

let order (nfa:nfa) : nfa = { start = nfa.start; accept = nfa.accept; trans = nfa.trans |> List.sortBy (fun (s1, sym, s2) -> (s1, symValue sym))}

let isSEps sym : bool = 
    match sym with
    | SEps -> true
    | SChar c -> false

let rec checkIsDFA list : bool =
    match list with
    | []    -> true
    | (s1, sym, s2)::xs -> List.filter (fun (s1X, symX, s2X) -> s1X=s1 && symX=sym) list |> List.length = 1 && not (isSEps sym) && checkIsDFA xs

let isDfa (nfa:nfa) : bool = checkIsDFA nfa.trans

let nextState = ref -1
let newState () = (nextState := 1 + !nextState; !nextState)

let fromState (state1, sym, state2) : state = state1
let toState (state1, sym, state2) : state = state2

let lastState (list : (state*sym*state) list) : state = list |> List.head |> toState
let firstState (list : (state*sym*state) list) : state = list |> List.rev |> List.head |> fromState

let makeNfa (re:re) : nfa =
    let rec stateList reNew list  : (state * sym * state) list =
        match reNew with 
        | Char re            -> ( newState (), SChar re, newState ())::list
        | Eps                -> ( newState (), SEps, newState ())::list
        | Seq (re1, re2)     -> let first = stateList re1 [] 
                                let second = stateList re2 []
                                second @ ( lastState first, SEps, firstState second)::first @ list
        | Star re            -> let states = stateList re list
                                (lastState states, SEps, firstState states)::states @ list
        | Choice (re1, re2)  -> let current = newState()
                                let first = stateList re1 []
                                let second = stateList re2 []
                                let last = newState()
                                [(lastState first, SEps, last);(lastState second, SEps, last)] @ second @ first @ (current, SEps, firstState second)::(current, SEps, firstState first)::list
    let states = stateList re []
    {start = firstState states; accept = lastState states; trans = states}
    