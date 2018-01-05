(*
 * Programming Languages, 2013 Fall.
 * Solution for Homework 1.
 * Joonwon Choi (jwchoi@ropas.snu.ac.kr)
 *)

(* Exercise 1: sigma *)
let rec sigma (a, b, f) =
  if a > b then 0
  else
    (f a) + (sigma (a+1, b, f))

(* Exercise 2: eval *)
type formula = 
  | TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr
and expr =
  | NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr
      
let rec eval f =
  match f with
    | TRUE -> true
    | FALSE -> false
    | NOT f -> not (eval f)
    | ANDALSO (f1, f2) -> (eval f1) && (eval f2)
    | ORELSE (f1, f2) -> (eval f1) || (eval f2)
    | IMPLY (f1, f2) -> (not (eval f1)) || (eval f2)
    | LESS (e1, e2) -> (evalExp e1) < (evalExp e2)
and evalExp e =
  match e with 
    | NUM i -> i
    | PLUS (e1, e2) -> (evalExp e1) + (evalExp e2)
    | MINUS (e1, e2) -> (evalExp e1) - (evalExp e2)

(* Exercise 3: natadd, natmul *)
type nat = ZERO | SUCC of nat

let rec natadd (n1, n2) =
  match n1 with
    | ZERO -> n2
    | SUCC s -> natadd (s, SUCC n2)
    
let rec natmul (n1, n2) =
  match n1 with
    | ZERO -> ZERO
    | SUCC s -> natadd (n2, natmul(s, n2))

(* Exercise 4: checkMetro *)
type metro =
  | STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string

let checkMetro metro =
  let rec check m lst = 
    match m with
      | STATION name -> List.mem name lst
      | AREA (name, m1) -> check m1 (name::lst)
      | CONNECT (m1, m2) ->
          (check m1 lst) && (check m2 lst)
  in
    check metro []

(* Exercise 5: merge *)
type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank h =
  match h with
    | EMPTY -> -1
    | NODE (r, _, _, _) -> r

let shake (x, lh, rh) =
  if (rank lh) >= (rank rh)
  then NODE(rank rh + 1, x, lh, rh)
  else NODE(rank lh + 1, x, rh, lh)

let rec merge (h1, h2) =
  match (h1, h2) with
    | (EMPTY, h)
    | (h, EMPTY) -> h
    | (NODE (_, x1, lh1, rh1), NODE (_, x2, lh2, rh2)) ->
	if x1 > x2
	then shake (x2, lh2, merge (h1, rh2))
	else shake (x1, lh1, merge (rh1, h2))

let findMin h =
  match h with
    | EMPTY -> raise EmptyHeap
    | NODE(_, x, _, _) -> x

let insert(x,h) =
  merge (h, NODE (0, x, EMPTY, EMPTY))

let deleteMin h =
  match h with
    | EMPTY -> raise EmptyHeap
    | NODE (_, x, lh, rh) -> merge (lh,rh)


(* End of the Solution for Homework 1 *)

