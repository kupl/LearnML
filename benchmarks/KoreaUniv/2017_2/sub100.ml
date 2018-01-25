(* problem 1 *)
  type btree = Empty | Node of int * btree * btree

  let rec mirror : btree -> btree
  = fun t -> match t with
  Empty -> Empty
  |Node(x,l,r) -> Node(x, mirror r, mirror l);;



  (* problem 2*)
  type nat = ZERO | SUCC of nat

  let rec natadd : nat -> nat -> nat 
  = fun n1 n2 ->
  match n2 with
  |ZERO -> let rec nat_ev n =
            match n with
            |ZERO -> ZERO
            |SUCC (n2) -> SUCC (nat_ev n2)
            in nat_ev n1
  |SUCC (n3) -> SUCC (natadd n1 n3);;

  let rec natmul : nat -> nat -> nat 
  = fun n1 n2 ->
   match n2 with
   |ZERO -> ZERO
   |SUCC (n3) -> natadd n1 (natmul n1 n3);;


  let rec natexp : nat -> nat -> nat 
  = fun n1 n2 -> 
    match n2 with
    |ZERO -> SUCC ZERO
    |SUCC(n3)->natmul n1 (natexp n1 n3);;

(*
  (* problem 3*)
  type formula =
      True
      | False
      | Var of string
      | Neg of formula
      | And of formula * formula
      | Or of formula * formula
      | Imply of formula * formula
      | Iff of formula * formula

       let sat : formula -> bool
       = fun f -> (* TODO *)
*)

  (* problem 4*)
  type aexp =
    | Const of int
    | Var of string
    | Power of string * int
     | Times of aexp list
    | Sum of aexp list
  let rec diff : aexp * string -> aexp
    = fun (e,x) ->
    match e with
    |Const a -> Const 0
    |Var y -> if x=y then Const 1 else Var y
    |Power (s, i) -> if s=x 
                     then Times[Const i;Power (s, i-1)]
                     else Power (s, i)
    (*not completed*)   
    |Sum m -> Const 0
    |Times l -> Const 0;;


  (* problem 5*)
  type exp = X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec calculator : exp -> int
  = fun e ->
  match e with
  |X -> 0
  |INT a -> a
  |ADD (a,b) -> calculator a + calculator b
  |SUB (a,b) -> calculator a - calculator b
  |MUL (a,b) -> calculator a * calculator b
  |DIV (a,b) -> if calculator b = 0
                then raise (Failure "0 problem")
                else calculator a / calculator b
  (*not completed*)
  |SIGMA (a,b,f) -> calculator a ;;

(*
  (* problem 6*)
  type mobile = branch * branch     (* left and rigth branches *)
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
             and length = int
             and weight = int

             let rec balanced : mobile -> bool
             = fun m ->
             match m with
             |
            
  (* problem 7*)
  type digit = ZERO | ONE
  type bin = digit list

  let bmul : bin -> bin -> bin
  = fun b1 b2 ->
  match b2 with
  |[hd::
*)
