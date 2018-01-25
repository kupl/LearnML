(*first homework*)


(* Problem 1 *)

let rec pascal : int * int -> int
=fun (x,y) -> match (x,y) with   
  |(0,0) -> 1
  |(-1,_) -> 0
  |(_,-1) ->0
  |_ -> pascal(x-1,y-1)+pascal(x-1,y) ;;



(* Problem 2 *)


let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> if a>b then 0
  else (f a)+(sigma f (a+1) b) ;;




(* Problem 3 *)

let rec max : int list -> int
=fun l -> match l with  
  |[]->raise(Failure "list is too short")
  |[x] -> x
  |hd::tl -> (if hd > (max tl) then hd    
  else (max tl)) 

let rec min : int list -> int
=fun l ->  match l with
  |[] -> raise(Failure "List is too short")
  |[x] -> x      
  |hd::tl -> (if hd < (min tl) then hd
  else (min tl));;



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
=fun f ->  match f with
      |True ->true
      |False ->false    
      |Neg f1 ->if eval f1 then false else true
      |Or (f1,f2) ->if eval f1 || eval f2 then true else false
      |And (f1,f2) -> if eval f1 && eval f2 then true else false
      |Imply (f1,f2) -> if (eval f1=true) && (eval f2=false) then false else true
      |Equiv (f1,f2) -> if (eval f1=true) && (eval f2=true) then true else if (eval f1=false)&&(eval f2=false) then true else false  ;;





(* Problem 5 *)

type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> match n1 with
	|ZERO->n2
	|SUCC(a)->SUCC(natadd a n2)

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> match n1 with
	|ZERO->ZERO
	|SUCC(a)->natadd (natmul a n2) n2 ;;





