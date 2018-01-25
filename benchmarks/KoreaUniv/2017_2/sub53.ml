(* problem 1*)
type btree = Empty | Node of int * btree * btree;;

let rec mirror : btree -> btree
= fun t -> 
match t with 
Empty -> t
| Node(a, b, c) -> Node(a, mirror c, mirror b);;


(* problem 2*)
type nat = ZERO | SUCC of nat;;

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> 
match n1 with 
ZERO -> n2
| SUCC a -> natadd a (SUCC n2);;


let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> 
match n1 with
ZERO -> ZERO
|SUCC ZERO -> n2
|SUCC a -> natadd n2 (natmul a n2);;

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> 
match n2 with
ZERO -> SUCC ZERO
| SUCC ZERO -> n1
| SUCC a -> natmul (natexp n1 a) n1;;


(* problem 3*)
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula;;

let rec proposition : formula -> string list
= fun f ->
match f with
True | False -> []
| Neg a -> proposition a
| And (a,b) | Or (a,b) | Imply (a,b) | Iff (a,b) -> proposition a @ proposition b
| Var x -> [x];;

 let rec unique x =
let rec uniq l n =
match l with
[] -> []
| h::t -> if n = h then uniq t n
else h::(uniq t n)
in match x with
[] -> []
|h::t -> h::(uniq (unique t) h);;

let prop f = unique (proposition f);;

let rec apply x e =
match e with
[] -> raise (Failure("ERROR"))
| (y,v)::tl -> if x = y then v else apply x tl;;

 let rec eval f e =
match f with
True -> true
| False -> false
| And(a,b) -> if eval a e && eval b e then true else false
| Or(a,b) -> if not (eval a e) && not (eval b e) then false else true
| Neg a -> if eval a e then false else true
| Imply(a,b) -> if eval a e && eval b e then true
else if not (eval a e) then true
else false
| Iff(a,b) -> if eval a e && eval b e then true
else if not (eval a e) && not (eval b e) then true
else false
| Var x -> eval (apply x e) e;;

let rec combinations = function
 | 0 -> [[]]
 | n ->
let rest = combinations (n-1) in
let comb_f = List.map (fun l -> False::l) rest in
let comb_t = List.map (fun l -> True::l) rest in
comb_t @ comb_f;;

let rec zip (a,b) =
match (a,b) with
([],[]) -> []
| ([], a::b) -> []
| (a::b, []) -> []
| (a::b, c::d) -> (a,c)::zip(b,d);;

let rec check f x =
match x with
[[]] -> true
| [] -> false
| hd::tl -> if eval f (zip(prop f,hd)) then true else check f tl;;


let sat : formula -> bool
= fun f -> check f (combinations (List.length (prop f)));;


(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list;;

  let rec exists_var : aexp * string -> bool
= fun (f,x) ->
match f with
| Sum[] -> false
| Times[] -> false
| Sum (hd::tl) -> if exists_var (hd, x) || exists_var (Sum tl, x) then true else false
| Var a -> if a = x then true else false
| Power (a,n) -> if a = x then true else false
| Times (hd::tl) -> if exists_var (hd, x) || exists_var (Times tl, x) then true else false
| Const n -> false;;

let rec find_element : aexp * string -> aexp
= fun (f,x) ->
match f with
| Sum[] -> Const 0
| Times[] -> Const 0
| Sum (hd::tl) -> if exists_var (hd,x) then Sum[find_element (hd,x); (find_element (Sum tl,x))] else find_element (Sum tl,x)
| Var a -> if a = x then Var a else Const 0
| Power (a,n) -> if a = x then Power (x,n) else Const 0
| Times lst -> if exists_var (Times lst, x) then Times lst else Const 0
| Const _ -> Const 0;;

let rec differ : aexp * string -> aexp
= fun (f,x) ->
match f with
| Sum[] -> Const 0
| Times [] -> Const 1
| Var a -> if a = x then Const 1 else Var a
| Power (a,n) -> if a = x then 
 if n = 1 then Const 1 else Times[Const n; Power(a, (n-1))]
 else Power (a,n)
| Times (hd::tl) -> Times[differ (hd,x); (differ (Times tl,x))]
| Sum (hd::tl) -> Sum[differ (hd,x); differ (Sum tl,x)]
| Const a -> Const a;;

let diff : aexp * string -> aexp
= fun (e,x) -> 
differ (find_element (e,x), x);;

(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp;;

let rec assign (x,n) =
match x with
SIGMA(a,b,c) -> assign (c,n)
| ADD(a,b) -> (assign (a,n)) + (assign (b,n))
| SUB(a,b) -> (assign (a,n)) - (assign (b,n))
| DIV(a,b) -> (assign (a,n)) / (assign (b,n))
| MUL(a,b) -> (assign (a,n)) * (assign (b,n))
| INT i -> i
| X -> n;;

let rec evalu x =
match x with
INT i-> i
| X -> raise(Failure("Error: Unassigned Variable"))
| ADD(a,b) -> (evalu a) + (evalu b)
| SUB(a,b) -> (evalu a) - (evalu b)
| MUL(a,b) -> (evalu a) * (evalu b)
| DIV(a,b) -> (evalu a) / (evalu b)
| SIGMA(a,b,c) -> if evalu a = evalu b then assign(c,evalu(a)) else assign(c,evalu(a))+evalu(SIGMA(INT(evalu(a)+1),b,c));;

let calculator : exp -> int
= fun e -> evalu e;;


(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int;;

let rec find_weight x =
match x with
CompoundBranch(a,(b,c)) -> (find_weight b) + (find_weight c)
| SimpleBranch(a,b) -> b;;

 let bal x =
match x with
| (CompoundBranch(a,(b,c)),CompoundBranch(d,(e,f))) -> if a*((find_weight b)+(find_weight c)) = d*((find_weight e)+(find_weight f)) then true else false
| (CompoundBranch(a,(b,c)),SimpleBranch(d,e)) -> if a*((find_weight b)+(find_weight c)) = d*e then true else false
| (SimpleBranch(a,b),CompoundBranch(c,(d,e))) -> if a*b = c*((find_weight d)+(find_weight e)) then true else false
| (SimpleBranch(a,b),SimpleBranch(c,d)) -> if a*b = c*d then true else false;;

let real x =
	match x with
| (CompoundBranch(a,(b,c)),CompoundBranch(d,(e,f))) -> if bal (b,c) && bal (e,f) then true else false
| (CompoundBranch(a,(b,c)),SimpleBranch(d,e)) -> if bal (b,c) then true else false
| (SimpleBranch(a,b),CompoundBranch(c,(d,e))) -> if bal (d,e) then true else false
| (SimpleBranch(a,b),SimpleBranch(c,d)) -> true;;

let balanced : mobile -> bool
= fun m -> if bal m && real m then true else false;;


(* problem 7*)
type digit = ZERO | ONE
type bin = digit list;;

let bmul : bin -> bin -> bin
= fun b1 b2 -> [ONE];;

let find_value = false;;


let rec add_bi x y =
match x with
[] -> []
| hd::tl ->
if List.length x > List.length y then [(hd, ZERO)]@add_bi tl y
else
match y with
[] -> []
| yh::yt -> [(hd,yh)]@ add_bi tl yt;;

let rec merge x =
match x with
[] -> []
| hd::tl ->
if hd = (ONE, ONE) then 2::merge tl
else if hd = (ONE, ZERO) || hd = (ZERO, ONE) then 1::merge tl
else 0::merge tl;;

let conver l =
match l with
[] -> []
| hd::tl -> if hd = 2 then [1;0]@tl
else hd::tl;;


