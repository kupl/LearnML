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

let rec evalex e =
match e with 
| Num n -> n
| Plus (e1, e2) ->
 begin
 let n1 = evalex e1 in
 let n2 = evalex e2 in
 n1 + n2
 end
| Minus (e1, e2) ->
 begin
 let n1 = evalex e1 in
 let n2 = evalex e2 in
 n1 - n2
 end

let rec eval f =
match f with
| True -> true
| False -> false
| Not f1 ->
 begin
 let b1 = eval f1 in
 if b1 == true then false else true
 end
| AndAlso (f1, f2) ->
 begin
 let b1 = eval f1 in
 let b2 = eval f2 in
 b1 && b2
 end
| OrElse (f1, f2) ->
 begin
 let b1 = eval f1 in
 let b2 = eval f2 in
 b1 || b2
 end
| Imply (f1, f2) -> 
 begin 
 let b1 = eval f1 in
 let b2 = eval f2 in
 if b1 == true && b2 == false then false else true
 end
| Equal (e1, e2) ->
 begin
 let n1 = evalex e1 in
 let n2 = evalex e2 in
 if n1 == n2 then true else false
 end

