(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t ->
match t with
 | Empty -> Empty
 | Node(a,b,c)->Node(a,mirror c,mirror b)
 


(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> 
match n1 with
 | ZERO -> n2
 | SUCC(t)->natadd (t) (SUCC(n2))

let rec natmul : nat -> nat -> nat 
= fun n1 n2 ->
match n1 with
 | ZERO -> ZERO
 | SUCC(ZERO) -> n2
 | SUCC(t) -> natadd (natmul t n2) n2

let rec natexp : nat -> nat -> nat 
= fun n1 n2 ->
match n2 with
 | ZERO -> SUCC(ZERO)
 | SUCC(ZERO) -> n1
 | SUCC(t) -> natmul n1 (natexp n1 t)


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
  = fun f ->
  let rec findvar : formula -> (formula * bool)list -> (formula * bool)list -> (formula * bool)list 
  = fun var l1 l2 ->
  match l1 with
  | [] -> [(var, true)]@l2
  | (a, b)::t -> if (a = var) then l2
          else findvar var t l2 in
  
  let rec findvar2 : formula -> (formula * bool) list -> bool 
  = fun var l -> 
  match l with
  | [] -> true
  | (a, b)::t -> if var = a then b
          else findvar2 var t in
  
  
  let rec mlist : formula -> (formula * bool) list -> (formula * bool) list
  = fun f l -> 
  match f with
  | True -> []
  | False -> []
  | Var a -> findvar (Var a) [] []
  | Neg a -> mlist a l
  | And (a, b) -> (mlist a l)@(mlist b l)
  | Or (a, b) -> (mlist a l)@(mlist b l)
  | Imply (a, b) -> (mlist a l)@(mlist b l)
  | Iff (a, b) -> (mlist a l)@(mlist b l) in
  
  let onepos = mlist f [] in
  
  let tchange : (formula * bool) list -> (formula * bool) list
  = fun l -> 
  match l with
  | [] -> []
  | (a, b)::t -> if b = true then ((a, false)::t) else t in
  
  let rec exetable : formula -> (formula * bool) list -> bool
  = fun f table -> 
  match f with
  | True -> true
  | False -> false
  | Var a -> findvar2 (Var a) table
  | Neg a -> not(exetable a table)
  | And (a, b) -> (exetable a table) && (exetable b table)
  | Or (a, b) -> (exetable a table) || (exetable b table)
  | Imply (a, b) -> if ((exetable a table) = true) && ((exetable b table) = false) then false else true
  | Iff (a, b) -> if (exetable a table) = (exetable b table) then true else false in
  
  let rec test : (formula * bool) list -> bool 
  = fun l -> 
  match l with
  | [] -> false
  | (a,b)::t -> b || (test t) in
  
  let rec exeall : formula -> (formula * bool) list -> bool 
  = fun f l -> 
  if (test l) = false then exetable f l
  else (exetable f l) || (exeall f (tchange l)) in
  exeall f onepos




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
|Const c -> Const 0
|Var v-> if(v=x) then Const 1 else Const 0
|Power (a,b) -> if((a=x)&&(b<>0)) then Times[Const b;Power (x, (b-1))] else Const 0
|Times e -> begin match e with
            |[] -> Const 0
            | h::t -> Sum[ Times ([diff (h, x)]@t) ; Times [ h ; diff (Times t, x)]]
end
|Sum l -> begin match l with
          |[]->Const 0
          |h::t->Sum [(diff (h,x));(diff (Sum t,x))]
end

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
 | INT n -> n
 | ADD (a,b)->(calculator a)+(calculator b)
 | SUB (a,b)->(calculator a)-(calculator b)
 | MUL (a,b)->(calculator a)*(calculator b)
 | DIV (a,b)->(calculator a)/(calculator b)
 | SIGMA (a,b,c)-> if((calculator a)>(calculator b)) then 0 else eval(c,a)+(calculator (SIGMA( ADD ( a, INT 1), b, c)))
 and eval (f,x)=
  match f with
  | X -> calculator x
  | INT n->n
  | ADD (a,b)->eval(a,x)+eval(b,x)
  | SUB (a,b)->eval(a,x)-eval(b,x)
  | MUL (a,b)->eval(a,x)*eval(b,x)
  | DIV (a,b)->eval(a,x)/eval(b,x)

(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec weight : mobile -> int
= fun n ->
match n with
SimpleBranch (a,b), SimpleBranch(c,d) -> b+d
|SimpleBranch (a,b), CompoundBranch(c,d) -> b+weight d
|CompoundBranch (a,b), SimpleBranch(c,d) -> weight b + d
|CompoundBranch (a,b), CompoundBranch(c,d) -> weight b + weight d
;;
let rec balanced : mobile -> bool
= fun m -> 
  match m with
  SimpleBranch (a,b), SimpleBranch(c,d) -> if(a*b=c*d) then true else false
  |SimpleBranch (a,b), CompoundBranch(c,d) -> if((a*b=c*(weight d))&&(balanced d)) then true else false
  |CompoundBranch (a,b), SimpleBranch(c,d) -> if((a*(weight b)=c*d)&&(balanced b)) then true else false
  |CompoundBranch (a,b), CompoundBranch(c,d) -> if((a*(weight b)=c*(weight d))&&(balanced b)&&(balanced d)) then true else false
;;


(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec fastexpt : int -> int -> int
= fun b n ->
if n=0 then 1
else if (n mod 2)=0 then (fastexpt b (n/2)) * (fastexpt b (n/2))
else (fastexpt b (n/2)) * (fastexpt b (n/2)) * b

let rec count : bin -> int
= fun a ->
match a with
[]->0
|h::t -> 1+count t

let rec decimalize : bin -> int -> int
= fun a b ->
match a with
[]->0
|h::t-> if(h=ONE) then (fastexpt 2 (b-1)) + (decimalize t (b-1)) else (decimalize t (b-1))

let rec binarylize : int -> bin -> bin
= fun a b ->
match a with
0 -> b@[]
|_ -> if((a mod 2) = 1) then (binarylize (a/2) b@[ONE]@b) else (binarylize (a/2) b)@[ZERO]@b

let bmul : bin -> bin -> bin
= fun b1 b2 ->
binarylize (decimalize b1 (count b1) * decimalize b2 (count b2)) []


