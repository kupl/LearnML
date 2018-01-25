(*problem 1*)
let rec fold f l a =
match l with
| [] -> a
| hd :: tl -> f hd (fold f tl a)

let rec max: int list -> int
	= fun l-> match l with
| h :: t -> 
 fold (fun a b -> if a>b then a else b) l h;;

let rec min: int list -> int
	= fun l -> match l with
| h :: t ->
 fold (fun a b -> if a<b then a else b) l h;;

(*problem 2*) 	
let rec filter: ('a -> bool) -> 'a list -> 'a list
= fun f l -> match l with
| [] -> []
| h :: t -> if f h then h :: (filter f t) else filter f t;;

(*problem 3*)
let rec double: ('a -> 'a) ->'a ->'a
  = fun f a ->
  f(f a)

(*problem 4*)
type btree =
 Empty
 |Node of int * btree *btree
 
 
 let rec mem : int -> btree -> bool
 = fun a b -> match b with
 |Empty -> false
 |Node(q,w,e) -> if a = q then true else mem a w || mem a e


(*problem 5*)
type nat = ZERO | SUCC of nat 

let rec natadd : nat -> nat -> nat
	= fun a b ->match a with
	|ZERO -> b
	|SUCC c -> SUCC(natadd c b) 

let rec natmul : nat -> nat -> nat
	=fun a b -> match b with
	|ZERO -> ZERO
	|SUCC c -> natadd a (natmul a c)

(*problem 6*)
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
 
 let rec pog k
 =match k with
 |Num q->q
 |Plus (q,w) -> pog q + pog w
 |Minus (q,w) -> pog q - pog w
 
 let rec eval : formula -> bool
 = fun f-> match f with
 |True -> true
 |False -> false
 |Not  a -> if eval a = true then false else true
 |AndAlso  (a,b) -> eval a && eval b
 |OrElse  (a,b) -> eval a || eval b
 |Imply  (a,b) -> if eval a = true && eval b = false  then false else true
 |Equal  (x,y) ->  
	if (pog x) =(pog y) then true else false

