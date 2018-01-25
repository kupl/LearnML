(* problem 1*)
type btree = Empty | Node of int * btree * btree

let mirror : btree -> btree
= fun t -> (* TODO *)
  match t with 
  |Empty -> Empty
  |Node(n,lNode,rNode) -> Node(n,(mirror rNode),(mirror lNode));;

(* problem 2*)
let rec natadd : nat -> nat -> nat = fun a b -> if a != ZERO then
  (match a with
    | ZERO -> ZERO
    | SUCC t -> SUCC (natadd t b))
  else 
  (match b with 
    | ZERO -> ZERO
    | SUCC t -> SUCC (natadd ZERO t));;

let rec natmul : nat -> nat -> nat = fun a b ->
  match a with
    | ZERO -> ZERO
    | SUCC t -> natadd (natmul t b) b;;

let rec natexp : nat -> nat -> nat = fun a b ->
  match b with
    | ZERO -> SUCC ZERO
    | SUCC t -> natmul a (natexp a t);;


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


(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let diff : aexp * string -> aexp
= fun (e,x) -> (* TODO *)


(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec eval : exp -> int -> int =
  fun ex env ->
  match ex with
  | X -> env
  | INT n -> n
  | ADD (a,b) -> (eval a env)+(eval b env)
  | SUB (a,b) -> (eval a env)-(eval b env)
  | MUL (a,b) -> (eval a env)*(eval b env)
  | DIV (a,b) -> (eval a env)/(eval b env)
  | SIGMA (n,m,e) -> if (eval n env) > (eval m env) then 0 else (eval e (eval n env)) + (eval (SIGMA ((INT((eval n env)+1)),m,e)) env);;

let calculator : exp -> int =
  fun ex -> eval ex 0;;


(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec calWeight : branch -> int = fun bl -> 
  match bl with
  | SimpleBranch(a,b) -> b
  | CompoundBranch(n,m) ->
    match m with
    | (a,b) -> (calWeight a) + (calWeight b);;

let rec balanced : mobile -> bool =
  fun m -> 
  match m with
  | (SimpleBranch(a,b),SimpleBranch(c,d)) -> if a * b = c * d then true else false
  | (SimpleBranch(a,b),CompoundBranch(c,d)) -> if (a * b = (c * (calWeight (CompoundBranch(c,d))))) then true && (balanced d) else false
  | (CompoundBranch(c,d),SimpleBranch(a,b)) -> if (a * b = (c * (calWeight (CompoundBranch(c,d))))) then true && (balanced d) else false
  | (CompoundBranch(a,b),CompoundBranch(c,d)) -> if ((a * (calWeight (CompoundBranch(a,b)))) = (c * (calWeight (CompoundBranch(c,d))))) then (true && (balanced b) && (balanced d)) else false;;


(* problem 7*)
type digit = ZERO | ONE;;
type bin = digit list;;

let rec reverse_list lst 
  = match lst with
    | [] -> []
    | h::t -> (reverse_list t)@[h];;

let rec numToBi : int -> int -> bin = fun n x -> 
  if n = 0 then [] else (if (n mod (x * 2)) = 0 then ZERO::(numToBi n (x * 2)) else ONE::(numToBi (n - (n mod (x * 2))) (x * 2)));;

let rec biToNum : bin -> int -> int = 
  fun lst x -> 
  match lst with
  | [] -> 0
  | h::t -> if h = ONE then (x + (biToNum t (x * 2))) else biToNum t (x * 2);;

let bmul : bin -> bin -> bin =
  fun a b -> reverse_list (numToBi ((biToNum (reverse_list a) 1) * (biToNum (reverse_list b) 1)) 1);;