open Regex
open Brzozowski

(* DFA construction using Brzozowski's derivatives *)
type state = re
type states = re list
type transition = (re * char * re) list

type dfa =
  { states : states
  ; start_state : re
  ; accept_state : re list
  ; transition : transition
  }

let alphabet : char list = List.init 256 Char.chr

let rec explore (info : states * transition) (state : state) : states * transition =
  List.fold_left (goto state) info alphabet

and goto (r : re) (info : states * transition) (c : char) : states * transition =
  let dr = bderiv r c in
  let states, transitions = info in
  if List.mem dr states then
    states, (r, c, dr) :: transitions
  else
    explore (dr :: states, (r, c, dr) :: transitions) dr

let build_dfa (r : re) : dfa =
  let start_state = r in
  let states, transition = explore ([ start_state ], []) start_state in
  let accept_state = List.filter nullable states in
  { states; start_state; accept_state; transition }
