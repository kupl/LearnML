let rec max l = 
	match l with 
	| [] -> raise(Failure "list is too short..")
	| hd :: tl ->if tl = [] then hd else
				 if hd > max tl then hd 
				else max tl

let rec min l = 
	match l with 
	| [] -> raise(Failure "list is too short..")
	| hd :: tl ->if tl = [] then hd else 
					 if hd < min tl then hd 
				else min tl

let rec filter f l =
 match l with 
 | [] -> []
 | hd :: tl -> if f hd = true then [hd] @ (filter f tl)
                 else filter f tl 

 let  double f x = f (f x )


 type btree = 
 | Empty 
 | Node of int * btree * btree 

let rec mem x tree = 
	match tree with
	 | Node(data,ltree,rtree) -> if x = data then true
	 							else if x < data then mem x rtree
	 							else mem x ltree			
	 | Empty -> false 


type nat = ZERO | SUCC of nat 
    
let rec natadd x y = 
	match x with 
	| ZERO  -> y 
	| SUCC (p) ->  SUCC ( natadd p y) 

let rec printnat x = 
	if x = 0 then ZERO 
	else  SUCC(printnat(x-1))




let rec xint x = 
	match x with 
	| ZERO -> 0
	| SUCC (p) -> 1+ xint p

let rec yint y = 
	match y with 
	| ZERO -> 0
	| SUCC (p) -> 1+ yint p

let rec natmul x y = 
	printnat ((xint x)*(yint y))

type formula = 
|True 
|False
|Not of formula
|AndAlso of formula * formula
|OrElse of formula * formula
|Imply of formula * formula
|Equal of exp * exp 

and exp = 
|Num of int 
|Plus of exp * exp
|Minus of exp * exp

let rec evalexp f =
match f with
|Num x -> x
|Plus (x,y)->(evalexp x)+(evalexp y)
|Minus (x,y)->(evalexp x)+(evalexp y)

let rec eval f = 
	match f with 
	| True -> true
	| False -> false 
	| Not (p) -> if (eval p) = true then false
				else true
	|AndAlso(p,q)-> if (eval p)=true &&(eval q) = true then true  
	                  else false
	|OrElse(p,q)->   if (eval p)=true ||(eval q) = true then true  
	                  else false 
	|Imply(p,q)-> if (eval q)=true then true 
				 else false
	|Equal(p,q)-> if(evalexp p)=(evalexp q) then true 
				else false                             




   