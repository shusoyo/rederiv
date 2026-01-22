open Regex

(* Brzozowski's derivatives for regular expressions *)

(** nullable is true if the regular expression can match the empty string *)
let rec nullable (r : re) : bool =
  match r with
  | Empty -> false
  | Epsilon -> true
  | Char _ -> false
  | Concat (r1, r2) -> nullable r1 && nullable r2
  | Star _ -> true
  | Or (r1, r2) -> nullable r1 || nullable r2
  | And (r1, r2) -> nullable r1 && nullable r2
  | Not r1 -> not (nullable r1)

let v (r : re) : re =
  if nullable r then
    Epsilon
  else
    Empty

(** derivative of a regular expression with respect to a character *)
let rec bderiv (r : re) (c : char) : re =
  match r with
  | Empty | Epsilon -> Empty
  | Char d ->
    if d = c then
      Epsilon
    else
      Empty
  | Concat (r1, r2) -> reOr (concat (bderiv r1 c) r2) (concat (v r1) (bderiv r2 c))
  | Star r1 -> concat (bderiv r1 c) (star r1)
  | Or (r1, r2) -> reOr (bderiv r1 c) (bderiv r2 c)
  | And (r1, r2) -> reAnd (bderiv r1 c) (bderiv r2 c)
  | Not r1 -> reNot (bderiv r1 c)

let bderivs (r : re) (s : string) : re = String.fold_left (fun acc c -> bderiv acc c) r s
let matches (r : re) (s : string) : bool = bderivs r s |> nullable
