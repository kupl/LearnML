(* problem 1*)
type btree = Empty | Node of int * btree * btree
 
let rec mirror : btree -> btree
= fun t -> 
  match t with
  | Empty -> Empty
  | Node (i, b1, b2) -> Node (i, mirror b2, mirror b1)
    
(* let t1 = Node (1, Empty, Empty)
let t2 = Node (1, Node(2, Node(3, Empty, Empty), Empty), Node(4, Empty, Empty))
let t3 = Node (1, Empty, Node(2, Empty, Empty))
let t4 = Empty
let t5 = Node (1,  Empty, Node(2, Node(3, Empty, Empty), Empty))

 *)

(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> 
  match n2 with    (* check the n2 first  *)
  | ZERO ->         (*  if n2 is ZERO, evaluate the value of n1 *)
    let rec evaln1 n =      (* evaluation function for n1  *)  
      match n with 
      | ZERO -> ZERO
      | SUCC n1_1 -> SUCC (evaln1 n1_1) (*  by tail recursive form, it evaluate n1 *)
    in evaln1 n1    (* evaluate n1 *)
  | SUCC n2_1 -> SUCC (natadd n1 n2_1)  
(* if n2 is not ZERO, tail recursively call natadd function by, 'evaln1', the evaluated value of n1 is accumulated. *)

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> 
  match n2 with
  | ZERO -> ZERO
  | SUCC n2_1 -> natadd n1 (natmul n1 n2_1) (* simply recall n2_1 times of (natadd n1)
 *)

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> 
  match n2 with
  | ZERO -> SUCC ZERO  (* any number's exp is one  *)
  | SUCC n2_1 -> natmul n1 (natexp n1 n2_1) (* call natmul by n2 times with n1 value*)


let one = SUCC ZERO;;
let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;
let four = SUCC (SUCC (SUCC (SUCC ZERO)));;


(* 
# natadd two three;;
- : nat = SUCC (SUCC (SUCC (SUCC (SUCC ZERO))))
# natmul two three;;
- : nat = SUCC (SUCC (SUCC (SUCC (SUCC (SUCC ZERO)))))
# natexp two three;;
- : nat = SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC ZERO)))))))
 *)


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


(* The implication p→q states that p implies q. That is, if p is true, then q is
true; but p is not true, then q could be either true or false. *)

let sat : formula -> bool
= fun f -> 
  let eval = 
  match f with
  | True -> true
  | False -> false
  | Var f -> env f
  | Neg f -> not (eval env f)
  | And (f1, f2) -> (eval env f1) && (eval env f2)
  | Or (f1, f2) -> (eval env f1) || (eval env f2)
  | Imply (f1, f2) -> not (eval env f1) || (eval env f2)
  | Iff (f1, f2) -> (eval env f1) = (eval env f2)   


let sat1 = sat (And (Var "P", Neg (Var "Q")))


let eval = 
  match f with
  | True -> true
  | False -> false
  | Var f -> env f
  | Neg f -> not (eval f env)
  | And (f1, f2) -> (eval f1 env) && (eval env f2)
  | Or (f1, f2) -> (eval f1 env) || (eval env f2)
  | Imply (f1, f2) -> not (eval f1 env) || (eval env f2)
  | Iff (f1, f2) -> (eval f1 env) = (eval env f2)   


 *)

(* 
(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let diff : aexp * string -> aexp
= fun (e,x) -> (* TODO *)
 *)



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
  let rec eval e v =
  match e with
  | X -> v
  | INT n -> n
  | ADD (e1, e2) -> 
    let v1 = eval e1 v 
    in let v2 = eval e2 v 
    in v1 + v2
  | SUB (e1, e2) -> 
    let v1 = eval e1 v 
    in let v2 = eval e2 v 
    in v1 - v2
  | MUL (e1, e2) -> 
    let v1 = eval e1 v 
    in let v2 = eval e2 v 
    in v1 * v2
  | DIV (e1, e2) -> 
    (match e2 with 
    | INT 0 -> raise (Failure (" cannot be divided by zero"))
    | _ -> let v1 = eval e1 v 
      in let v2 = eval e2 v 
      in v1 / v2)
  | SIGMA(x1, x2, e) -> 
    let v1 = eval x1 v
    in let v2 = eval x2 v
    in let rec sigma_inter x = 
      if x = v1 then eval e x
      else eval e x + sigma_inter (x-1) 
    in sigma_inter v2
  in eval e 0


(* 
let exp0 = SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1))
let exp1 = INT 1
let exp2 = ADD (INT 2, INT 1)
let exp3 = ADD (INT 3, INT 1)
let exp4 = SUB (INT 2, INT 1)
let exp5 = SUB (INT 3, INT 1)
let exp6 = MUL (INT 3, INT 4)
let exp7 = DIV (INT 3, INT 0)
let exp8 = SIGMA (INT 1, INT 2, ADD (X, INT 1))
 *)





(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m -> 
 let rec sum e = 
  match e with
  | (SimpleBranch(l1,w1), SimpleBranch(l2,w2)) ->
    w1+w2
  | (SimpleBranch(l1,w1), CompoundBranch(l2,m2)) ->
    w1 + (sum m2)
  | (CompoundBranch(l1, m1), SimpleBranch(l2,w2)) ->
    (sum m1) + w2
  | (CompoundBranch(l1,m1), CompoundBranch(l2,m2)) ->
    (sum m1) + (sum m2)
  in let rec balance e = 
  match e with
  | (SimpleBranch(l1,w1), SimpleBranch(l2,w2)) ->
    if l1 * w1 != l2 * w2 then false
    else true
  | (SimpleBranch(l1,w1), CompoundBranch(l2,m2)) ->
    if l1 * w1 != l2 * (sum m2) then false
    else balance m2
  | (CompoundBranch(l1, m1), SimpleBranch(l2,w2)) ->
    if l1 * (sum m1) != l2 * w2 then false
    else balance m1
  | (CompoundBranch(l1,m1), CompoundBranch(l2,m2)) ->
    if l1 * (sum m1) != l2 * (sum m2) then false
    else balance m1 && balance m2
  in balance m



(* 
(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> (* TODO *)
 *)