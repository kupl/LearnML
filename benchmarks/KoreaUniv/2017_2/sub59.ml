(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t ->
    match t with
    |Empty -> Empty
    |Node(x,Empty,Empty) -> Node(x,Empty,Empty)
    |Node(x,lt,rt) -> Node(x,mirror rt,mirror lt);;


(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
    match n2 with
    |ZERO -> n1
    |SUCC ZERO -> SUCC(n1)
    |SUCC(n) -> SUCC(natadd n1 n);;

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
    match n2 with
    |ZERO -> ZERO
    |SUCC ZERO -> n1
    |SUCC(n) -> natadd (natmul n1 n) n1;;


let rec natexp : nat -> nat -> nat
= fun n1 n2 ->
    match n2 with
    |ZERO -> SUCC ZERO
    |SUCC ZERO -> n1
    |SUCC(n) -> natmul (natexp n1 n) n1;;


open Printf
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

module VarMap = Map.Make(String)

let add_to_map a m =
VarMap.add a false m

let first_el k xo yo =
match xo,yo with
| Some x, Some y -> Some x
| None, yo -> yo
| xo, None -> xo

let get_1 (a,_) = a

let sat : formula -> bool = fun f ->
let rec build_map f m =
match f with
|Var(x) -> add_to_map x m
|And(x1, x2) -> VarMap.merge first_el (build_map x1 m) (build_map x2 m)
|Or(x1, x2) -> VarMap.merge first_el (build_map x1 m) (build_map x2 m)
|Imply(x1, x2) -> VarMap.merge first_el (build_map x1 m) (build_map x2 m)
|Iff(x1, x2) -> VarMap.merge first_el (build_map x1 m) (build_map x2 m)
|Neg(x) -> build_map x m
|True -> m
|False -> m in
let rec eval_formula f m =
match f with
|Var(x) -> VarMap.find x m
|And(x1, x2) -> (eval_formula x1 m) && (eval_formula x2 m)
|Or(x1, x2) -> (eval_formula x1 m) || (eval_formula x2 m)
|Imply(x1, x2) -> eval_formula (Or(Neg(x1), x2)) m
|Iff(x1, x2) -> eval_formula (Or(And(x1, x2),And(Neg(x1),Neg(x2)))) m
|Neg(x) -> not (eval_formula x m)
|True -> true
|False -> false in
let rec loop f l m =
if VarMap.is_empty l then eval_formula f m
else if loop f (VarMap.remove (get_1 (VarMap.choose l)) l) m then true
else if loop f (VarMap.remove (get_1 (VarMap.choose l)) l) (VarMap.add (get_1 (VarMap.choose l)) true m) then true
else false in
let map = VarMap.empty in
let map = build_map f map in
loop f map map


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
    |Const y -> Const 0
    |Var s -> if s = x then Const 1 else Var s
    |Times al -> begin
        match al with
            |hd::tl ->
                Sum[Times[diff (hd,x); Sum tl]; diff (Sum tl,x)]
        end
    |Power(s,n) ->
            if s=x then Times [Const n; Power (s,n-1)] else Power(s,n)
    |Sum m -> match m with
            |[] -> Const 0
            |hd::tl -> Sum[diff (hd,x);diff (Sum tl,x)]


(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec new_fun : int -> exp -> int = fun i e ->
    match e with
    | X -> i
    | INT x -> x
    | ADD(e1, e2) -> new_fun i e1 + new_fun i e2
    | SUB(e1, e2) -> new_fun i e1 - new_fun i e2
    | MUL(e1, e2) -> new_fun i e1 * new_fun i e2
    | DIV(e1, e2) -> new_fun i e1 / new_fun i e2


let rec calculator : exp -> int
= fun e ->
    match e with
    | INT x -> x
    | ADD(e1, e2) -> calculator e1 + calculator e2
    | SUB(e1, e2) -> calculator e1 - calculator e2
    | MUL(e1, e2) -> calculator e1 * calculator e2
    | DIV(e1, e2) -> calculator e1 / calculator e2
    | SIGMA(srt,n,e) ->
        if (calculator srt) = (calculator n) then new_fun (calculator n) e
        else new_fun (calculator n) e + calculator (SIGMA(srt,SUB(n,INT 1),e))


(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec mobile_weight : mobile -> int = fun m ->
    match m with
    |b1,b2 -> (match b1 with
                |SimpleBranch(l1,w1) -> (match b2 with
                                        |SimpleBranch(l2,w2) -> w1 + w2
                                        |CompoundBranch(l2,m) -> w1 + mobile_weight m
                                        |_->0
                                        )
                |CompoundBranch(l1,m1)-> (match b2 with
                                        |SimpleBranch(12,w2) -> mobile_weight m1 + w2
                                        |CompoundBranch(12,m2) -> mobile_weight m1 + mobile_weight m2
                                        |_->0
                                        ))

let rec balanced : mobile -> bool
= fun m ->
    match m with
    |b1, b2 -> (match b1 with
            |SimpleBranch(l1,w1) -> (match b2 with |SimpleBranch(l2,w2) -> l1*w1 = l2*w2
                                                    |CompoundBranch(l2,m) -> (l1*w1 = (mobile_weight m)*l2) && balanced m
                                                    )
            |CompoundBranch(l1,m1) -> (match b2 with |SimpleBranch(l2, w2) -> (l1*(mobile_weight m1) = l2*w2) && balanced m1
                                                    |CompoundBranch(l2,m2) -> (l1*(mobile_weight m1) = l2*(mobile_weight m2) ) && balanced m2 && balanced m1
                                                    ))

(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec bi2int : bin -> int = fun bi ->
    if List.length bi = 1 then
        match bi with
        |hd::tl -> if hd = ZERO then 0 else 1
    else
        match bi with
        |hd::tl ->
            match hd with
            |ZERO -> 2 * bi2int(tl)
            |ONE -> 2 * bi2int(tl) + 1

let rec int2bi : int -> bin -> bin = fun i bi ->
    if i = 0 then bi
    else if i mod 2 = 0 then int2bi (i/2) ([ZERO]@bi) else int2bi (i/2) ([ONE]@bi)

let bmul : bin -> bin -> bin
= fun b1 b2 ->
    let b1 = List.rev b1
    in let b2 = List.rev b2
    in int2bi ((bi2int b1) * (bi2int b2)) [];;

