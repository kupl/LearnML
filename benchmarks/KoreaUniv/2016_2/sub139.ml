(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> let h =
match lst with 
|[]->0
|hd::tl -> hd in
let rec fold f l b =
match l with 
|[]->h
|hd::tl -> f hd (fold f tl h) in
fold(fun x y -> if(x>y) then x else y) lst h;;


let rec min : int list -> int
= fun lst -> let h =
match lst with
|[]->0
|hd::tl -> hd in
let rec fold f l b =
match l with
|[]->h
|hd::tl -> f hd (fold f tl h) in
fold (fun x y -> if(x>y) then y else x) lst h;;


(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
let rec fold f l =
match l with
|[]->[]
|hd::tl-> if(f hd) then hd::(fold f tl) else fold f tl  in
fold pred lst;;


(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f(f a);;


(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
|Empty -> false
|Node(x,n1,n2) -> if(n=x) then true else mem n n1||mem n n2;;


(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> let total =
let rec length nt =
match nt with
|ZERO -> 0
|SUCC ZERO -> 1
|SUCC(x) -> 1+(length x) in
(length n1)+(length n2) in
let rec makenat t =
match t with
|0 -> ZERO
|1 -> SUCC ZERO
|_ -> SUCC (makenat (t-1)) in
makenat total;;


let rec natmul : nat -> nat -> nat
= fun n1 n2 -> let total =
let rec length nt =
match nt with
|ZERO -> 0
|SUCC ZERO -> 1
|SUCC(x) -> 1+(length x) in
(length n1)*(length n2) in
let rec makenat t =
match t with
|0 -> ZERO
|1 -> SUCC ZERO
|_ -> SUCC (makenat (t-1)) in
makenat total;;

(*********************)
(*     Problem 6     *)
(*********************)
type formula =
	| True
	| False 
	| Not of formula 
	| AndAlso of formula * formula 
	| OrElse of formula * formula 
	| Imply of formula * formula 
	| Equal of exp * exp

and exp = 
	| Num of int 
	| Plus of exp * exp 
	| Minus of exp * exp 

let rec eval : formula -> bool
= fun f -> let rec foldeval b=
match b with
|Num i -> i
|Plus(l,m)-> (foldeval l)+(foldeval m)
|Minus(l,m) -> (foldeval l)-(foldeval m) in
let rec fold a =
match a with
|True -> True
|False -> False
|Not x1 -> if (fold x1 = True) then False else True
|AndAlso (x2,y2) -> if(fold x2=True&&fold y2=True) then True else False
|OrElse (x3,y3) -> if(fold x3=True||fold y3=True) then True else False
|Imply (x4,y4) -> if(fold x4=False) then True else if(y4=True) then True else False
|Equal (l5,m5) -> if(foldeval l5=foldeval m5) then True else False in
if((fold f)=True) then true else false;;
