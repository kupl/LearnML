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

                  
let rec c2N = fun exp ->
  match exp with
    |Num n' -> n'
    |Plus(x',y')-> c2N x'+c2N y'
    |Minus(x',y')-> c2N x'-c2N y';;
      
let reverse = fun x -> (* Not대신 사용*)
  if x =true then False else True;;
  
let rec eval : formula -> bool
= fun f ->
  match f with
    |True -> true
    |False -> false
    |Not a'-> eval(reverse (eval a'))
    |Equal (a',b') -> if c2N a' = c2N b' then true else false
    |Imply(a',b')-> if eval a' = true && eval b' = false then false else true
    |AndAlso (a',b')-> eval a' && eval b'
    |OrElse(a',b')-> eval a' || eval b';;
    
eval(Equal(Plus(Num 1, Num 1),Minus(Num 4,Num 2)));;
eval(Imply (Imply (True,False), True));;
