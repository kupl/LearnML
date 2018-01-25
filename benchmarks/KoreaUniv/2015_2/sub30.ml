(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst =
  match lst with
  | [] -> []
  | hd :: tl ->
      let new_tl = filter pred tl in
      if pred hd then hd :: new_tl else new_tl
;;

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) ->
  match a, b with
  | _, [] -> a
  | [], _ -> b
  | hda :: tla, hdb :: tlb ->
      if hda < hdb then hda :: zipper(tla,b) else hdb :: zipper(a,tlb)
;;

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) x ->
  if n <= 0 then
    x
  else
    f (iter(n-1, f) x)
;;

(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) ->
  match aexp with
  | Const(a) -> Const 0
  | Var(a) -> if a = x then Const 1 else Const 0
  | Power(a, b) ->
      if a = x then
        Times( (Const b)::[(Power(a, b-1))])
      else
        Const 0
  | Times(l) ->
      (
        match l with
        | [] -> Const 0
        | hd::[] -> diff(hd, x)
        | hd::tl -> Sum(Times(hd::[diff(Sum(tl), x)])::[Times(diff(hd, x)::tl)])
      )
  | Sum(l) ->
      let iterSum a =
        diff(a, x)
      in
      Sum(List.map iterSum l)
;;

(*************************)
(* Problem 5: Calculator *)
(*************************)
type exp = X
| INT of int
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
let rec calculator : exp -> int
=fun e ->
  match e with
  | X -> raise (Failure "invalid argument")
  | INT(x) -> x
  | ADD(x, y) -> (calculator x) + (calculator y)
  | SUB(x, y) -> (calculator x) - (calculator y)
  | MUL(x, y) -> (calculator x) * (calculator y)
  | DIV(x, y) -> (calculator x) / (calculator y)
  | SIGMA(x, y, z) ->
      let x1 = (calculator x) and y2 = (calculator y) in
      if x1 > y2 then 0 else  eval(z, x) + calculator (SIGMA (ADD (x, INT 1), y,
      z))
    and eval (f,x) =
      match f with
      | X -> calculator x
      | INT z -> z
      | ADD (a, b) -> eval (a,x) + eval (b,x)
      | SUB (a, b) -> eval (a,x) - eval (b,x)
      | MUL (a, b) -> eval (a,x) * eval (b,x)
      | DIV (a, b) -> eval (a,x) / eval (b,x)
      | SIGMA (i, j, a) -> calculator (SIGMA (INT (eval (i, x)), INT (eval (j,
      x)), a))
;;
