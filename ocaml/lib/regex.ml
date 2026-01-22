let equal_char = Base.equal_char
let compare_char = Base.compare_char

(** type definition of Regular expressions *)
type re =
  (* basic operators *)
  | Empty (* empty set *)
  | Epsilon (* empty string *)
  | Char of char (* single character *)
  | Concat of re * re (* concatenation *)
  | Star of re (* Kleene star *)
  (* Bool operators *)
  | Or of re * re (* alternation *)
  | And of re * re (* conjunction *)
  | Not of re (* negation *)
[@@deriving equal, compare]

(** smart constructors *)
let empty : re = Empty

let char (c : char) : re = Char c
let epsilon : re = Epsilon

(* not implemented associativity *)
let rec concat (r1 : re) (r2 : re) : re =
  match r1, r2 with
  | Empty, _ | _, Empty -> Empty
  | Epsilon, _ -> r2
  | _, Epsilon -> r1
  | Concat (s1, s2), _ -> concat s1 (concat s2 r2)
  | _, Concat (s1, s2) ->
    if compare_re r1 s1 > 0 then
      concat s1 (concat r1 s2)
    else
      Concat (r1, r2)
  | s1, s2 when compare_re s1 s2 > 0 -> concat s2 s1
  | _ -> Concat (r1, r2)

let star (r : re) : re =
  match r with
  | Empty | Epsilon -> Epsilon
  | Star _ -> r
  | _ -> Star r

let rec reOr (r1 : re) (r2 : re) : re =
  match r1, r2 with
  | Empty, _ -> r2
  | _, Empty -> r1
  | Not Empty, _ | _, Not Empty -> Not Empty
  (* Or operator is right associativity *)
  | Or (s1, s2), _ -> reOr s1 (reOr s2 r2)
  | _, Or (s1, s2) ->
    if compare_re r1 s1 > 0 then
      reOr s1 (reOr r1 s2)
    else
      Or (r1, r2)
  | _ ->
    if r1 = r2 then
      r1
    else if compare_re r1 r2 > 0 then
      Or (r2, r1)
    else
      Or (r1, r2)

let rec reAnd (r1 : re) (r2 : re) : re =
  match r1, r2 with
  | Empty, _ | _, Empty -> Empty
  | Not Empty, _ -> r2
  | _, Not Empty -> r1
  (* And operator is right associativity *)
  | And (s1, s2), _ -> reAnd s1 (reAnd s2 r2)
  | _, And (s1, s2) ->
    if compare_re r1 s1 > 0 then
      reAnd s1 (reAnd r1 s2)
    else
      And (r1, r2)
  | _ ->
    if r1 = r2 then
      r1
    else if compare_re r1 r2 > 0 then
      And (r2, r1)
    else
      And (r1, r2)

let reNot (r : re) : re =
  match r with
  | Not r1 -> r1
  | _ -> Not r
