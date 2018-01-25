(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a =
	begin 
	match l with
	| [] -> a
	| hd::tl -> f hd (fold f tl a)
	end;;

let rec max : int list -> int
= fun lst -> fold (fun x y -> if x>y then x else y) lst 0;;  (* TODO *)

let rec min : int list -> int
= fun lst -> fold (fun x y -> if x>y then y else x) lst (max lst);;  (* TODO *)

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
begin
	match lst with
	| [] -> []
	| hd::tl -> if pred hd then hd::(filter pred tl) else filter pred tl
end;; 
(* TODO *)

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f(f a);; (* TODO *)

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->
match tree with
| Empty -> false
| Node(p,q,r) -> if (p=n) then true else if (mem n q) then true else if (mem n r) then true else false;; 
  (* TODO *)

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->  (* TODO *)
match n1 with
|ZERO -> n2
|SUCC(k)-> SUCC(natadd k n2);;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
match n1 with 
|ZERO -> ZERO
|SUCC(k)-> natadd n2 (natmul k n2) ;; (* TODO *)

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
= fun f ->
 
match f with
|True -> true
|False -> false
|Not(k) -> if(eval(k)=eval(True)) then false else true
|AndAlso(p,q) -> if((eval(p)=eval(True))&&(eval(q)=eval(True))) then true else false
|OrElse(p,q) -> if((eval(p)=eval(False))&&(eval(q)=eval(False))) then false else true
|Imply(p,q)-> if(eval(p)=eval(True)&&eval(q)=eval(False)) then false else true
|Equal(p,q) -> if((eval2 p)=(eval2 q)) then true else false

and eval2
= fun g ->
begin
match g with
|Num(i) -> i
|Plus(j,k)->eval2(j)+eval2(k)
|Minus (j,k) -> eval2(j)-eval2(k)
end;;

  

 (* TODO *)

