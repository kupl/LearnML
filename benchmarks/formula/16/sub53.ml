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
   | True -> true
   | False -> false
   | Not fo -> if eval fo = true then false else true
   | AndAlso (fo1, fo2) -> if eval fo1 = true && eval fo2 = true then true else false
   | OrElse (fo1, fo2) -> if eval fo1 = false && eval fo2 = false then false else true
   | Imply (fo1, fo2) -> if eval fo1 = true && eval fo2 = false then false else true
   | Equal (ex1, ex2) -> 
   	let rec eti e = 
   	match e with 
       |Num k -> k
       |Plus (k, l) -> (eti k) + (eti l)
       |Minus (k, l) -> (eti k) - (eti l) in
      if (eti ex1) = (eti ex2) then true else false;;
