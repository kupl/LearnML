(* problem 1 *)
type btree = Empty | Node of int * btree * btree

let mirror : btree->btree
=fun t -> let rec f t = 
match t with 
 | Empty -> Empty
 | Node(int,tree1,tree2) -> Node(int, f tree2, f tree1)
in f t

(* problem 2 *)
type nat = ZERO | SUCC of nat

let natadd : nat->nat->nat
= fun n1 n2 -> let rec f n1 n2 =
  match n2 with
   ZERO -> n1
  |SUCC n -> f (SUCC n1) n
  in f n1 n2

let natmul : nat->nat->nat
= fun n1 n2 -> let rec f n1 n2 =
match n2 with
 ZERO -> ZERO 
 |SUCC n -> natadd n1 (f n1 n)
 in f n1 n2

let natexp : nat->nat->nat
= fun n1 n2 -> let rec f n1 n2 = 
match n2 with
 ZERO -> SUCC ZERO
|SUCC n -> natmul n1 (f n1 n)
  in f n1 n2

(* problem 3 *)


(* problem 4 *)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let diff : aexp * string -> aexp
=fun (e,x) -> let rec func aexp x =
  match aexp with
  | Const _ -> Const 0
  | Var y -> if x=y then Const 1 else Const 0
  | Power (y,n) -> if x=y then Times [Const n; Power (y, n-1)] else Const 0
  | Times lst ->
   ( match lst with
     | [] -> raise (Failure "fail")
     | [t] -> raise (Failure "fail")
     | [t1;t2] -> Sum [Times [func t1 x; t2]; Times [t1; func t2 x]]
     | hd::tl -> Sum [Times [func hd x; Times tl]; Times [hd; func (Times tl) x]])
  | Sum lst ->
    ( match lst with
      | [] -> raise (Failure "fail")
      | [s] -> raise (Failure "fail")
      | [s1;s2] -> Sum [func s1 x; func s2 x]
      | hd::tl -> Sum [func hd x; func (Sum tl) x])
in func e x

(* problem 5 *)
type exp = X
        | INT of int
        | ADD of exp * exp
        | SUB of exp * exp
        | MUL of exp * exp
        | DIV of exp * exp
        | SIGMA of exp * exp * exp

let calculator : exp -> int
= fun e -> let rec func exp =
match exp with
  | X -> raise (Failure "fail")
  | INT num->num
  | ADD (e1, e2) -> func e1 + func e2
  | SUB (e1, e2) -> func e1 - func e2
  | MUL (e1, e2) -> func e1 * func e2
  | DIV (e1, e2) -> func e1 / func e2
  | SIGMA (e1, e2, e3) ->
   let a,b = func e1, func e2 in
    if a = b then evaluate e3 a 
    else if a < b then evaluate e3 a + func ( SIGMA (INT (a+1), INT b, e3))
    else raise (Failure "error")
  and  evaluate : exp -> int -> int
  = fun exp n ->
  match exp with
  | X -> n
  | INT n -> n
  | ADD (e1, e2) -> evaluate e1 n + evaluate e2 n
  | SUB (e1, e2) -> evaluate e1 n - evaluate e2 n
  | MUL (e1, e2) -> evaluate e1 n * evaluate e2 n
  | DIV (e1, e2) -> evaluate e1 n / evaluate e2 n
  | SIGMA _ -> func exp

in func e








