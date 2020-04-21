type formula = True
| False
| Not of formula
| AndAlso of formula * formula
| OrElse of formula * formula
| Imply of formula * formula
| Equal of exp * exp
and exp = Num of int
| Plus of exp * exp
| Minus of exp * exp

let eval : formula -> bool = fun a ->
   match a with 
   |True -> true
   |False -> false
   |(Not True) -> false
   |(Not False) -> true
   |AndAlso(True, True) -> true
   |AndAlso(True,False) -> false
   |AndAlso(False, True) -> false
   |AndAlso(False, False) -> false
   |OrElse(True, True) -> true
   |OrElse(True, False) -> true
   |OrElse(False, True) -> true
   |OrElse(False, False) -> false
   |Imply(True, False) -> false
   |Imply(True, True) -> true
   |Imply(False, True) -> true
   |Imply(False, False) -> true
   |Equal(Num b,Num c) ->
     if b > c then false
     else true
   |Equal(Plus(Num b, Num c), Plus(Num d, Num e)) ->
     let x = b + c
     and y = d + e
     in
       if x > y then false
       else true
   |Equal(Plus(Num b, Num c), Minus(Num d, Num e)) ->
     let x = b + c
     and y = d - e
     in
       if x > y then false
       else true
   |Equal(Minus(Num b, Num c), Plus(Num d, Num e)) ->
     let x = b - c
     and y = d + e
     in
       if x > y then false
       else true
   |Equal(Minus(Num b, Num c), Minus(Num d, Num e)) ->
     let x = b - c
     and y = d - e
     in
       if x > y then false
       else true

