(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> 
	match (x,y) with
	|(0,0) -> 1
	|(x,y) -> if (y=0 || x=y) then 1 else  pascal(x-1,y-1) + pascal(x-1,y);;

pascal (0,0);;
pascal (1,0);;
pascal (1,1);;
pascal (2,1);;
pascal (4,2);;

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> if a=b then f a else f a + sigma f (a+1) b ;;

sigma (fun x -> x) 1 10;;
sigma (fun x -> x*x) 1 7;;

(* Problem 3 *)

exception NoElement;;


let rec max : int list -> int
=fun l -> match l with
					|[]-> raise NoElement
					|[hd] -> hd
					|hd::tl -> if (hd > max tl) then hd else max tl;;

let rec min : int list -> int
=fun l -> match l with
					|[]-> raise NoElement
					|[hd] -> hd
					|hd::tl -> if (hd < min tl) then hd else min tl;;

max [1;3;5;2];;
min [1;3;2];;


(* Problem 4 *)
type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula

let rec eval : formula -> bool
=fun f -> match f with
					|True -> true
					|False -> false
					|Neg a -> (match a with
										|True->false
										|False->true
										|_-> if eval a = true then false else true)
					|Or (a,b) -> (match (a,b) with
											|(True,True)->true
											|(True,False)->true
											|(False,True)->true
											|(False,False)->false
											|_->if (eval a = true || eval b = true) then true else false)
					|And (a,b) -> (match (a,b) with
											|(True,True)->true
											|(True,False)->false
											|(False,True)->false
											|(False,False)->false
										  |_->if (eval a = true && eval b = true) then true else false)

					|Imply (a,b) -> (match (a,b) with
												|(True,True)->true
												|(True,False)->false
												|(False,True)->true
												|(False,False)->true
												|_->if (eval a = true && eval b = false) then false else true)
					|Equiv (a,b) -> (match (a,b) with
													|(True,True)->true
													|(True,False)->false
													|(False,True)->false
													|(False,False)->true
													|_->if eval a = eval b then true else false);;


	

eval (Or (True,False));;


(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> match n1 with
							|ZERO -> n2
							|SUCC (n3) -> natadd n3 SUCC(n2);;
(*
let rec natmul : nat -> nat -> nat
=fun n1 n2 -> match n1 with
							|ZERO -> ZERO
							|SUCC(ZERO) -> n2
							|SUCC(n3) -> natmul n3 (natadd n2 n2);;*)
