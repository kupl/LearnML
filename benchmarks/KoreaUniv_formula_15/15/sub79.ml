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
True -> true
|False -> false
|Neg f -> if (eval f)=true then false
          else true
|Or (a,b) -> if (eval a) = true then true
             else if (eval b) = true then true
             else false
|And (a,b) -> if (eval a) = false then false
              else if (eval b) = false then false
              else true
|Imply (a,b) -> if (eval a) = false then true
                else if (eval b) = true then true
                else false
|Equiv (a,b) -> if (eval a)=(eval b) then true
                else false
