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
