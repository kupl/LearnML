(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a =
match l with
|[]->a
|hd::tl -> f hd (fold f tl a)

let rec map f l =
match l with
|[]->[]
|hd::tl -> (f hd) ::(map f tl)

let length l = fold(fun x y -> 1+y) l 0

let rec last l = 
match l with
|[a]->a
|_::tl->last tl
|[]->0

let rec max : int list -> int
= fun lst -> fold (fun x y ->if x>y then x else y) lst (last lst)
let rec min : int list -> int
= fun lst -> fold (fun x y ->if x<y then x else y) lst (last lst)
(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
match lst with
|[]->[]
|hd::tl->if (pred hd)=true then hd::(filter pred tl) else filter pred tl
(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = 
f (f a) 

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> 
match tree with
|Empty ->false
|Node(i,b1,b2)->if i=n then true
								else (mem n b1)||(mem n b2)

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat 

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
match n1 with
|ZERO -> n2
|SUCC(n1')-> SUCC(natadd n1' n2)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
match n1, n2 with
|ZERO, n2 -> ZERO
|SUCC ZERO, n2 -> n2
|SUCC (n1'), n2 -> natadd n2 (natmul (n1') n2) 
											 
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

let rec evalexp exp =
match exp with
|Num n  ->n
|Plus(n1, n2) -> 
								(match n1,n2 with
							(*	|Num(n1'), Num(n2') -> (evalexp(n1')+evalexp(n2'))*)
								|_, _ ->evalexp(n1)+evalexp(n2))
|Minus(n1, n2) -> (match n1, n2 with
								(*	|Num(n1'), Num(n2')->(evalexp(n1')-evalexp(n2'))*)
									|_,_ ->evalexp(n1)-evalexp(n2))

let rec eval : formula -> bool
= fun f -> 
match f with
|True -> true
|False -> false
|Not(f') ->( match f' with
						|True -> false
						|False ->true
						|_ -> eval(f')
						)					
|AndAlso(f1', f2') ->( match (f1', f2') with
											|(True, True) -> true
											|(False, _) -> false
											|(_, False)-> false
											|(_, _) -> ((eval f1'&&eval f2')))
|OrElse(f1', f2') ->( match f1', f2' with
										|True, True -> true
										|True, False -> true
										|False, True-> true
										|False, False -> false	
										|_,_ ->((eval f1'||eval f2')))								
|Imply(f1',f2') ->( match f1', f2' with
									|True, True -> true
									|True, False -> false
									|False , True -> true
									|False , False -> true
									|_, _ -> ((eval (Not(f1')))||eval f2')
									)
|Equal(exp1, exp2)->
	 (match evalexp(exp1),evalexp(exp2) with 
	|a,b -> if a = b then true else false
	)
