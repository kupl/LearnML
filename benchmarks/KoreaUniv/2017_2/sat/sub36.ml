(*Problem 3*)
type formula =
  | True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

let sat : formula -> bool
= fun f ->
  let rec union l1 l2 = 
    match l2 with
    | x::xs -> if List.mem x l1 
               then union l1 xs
               else x::(union l1 xs)
    | [] -> l1
  in
  let rec formulaToVar formula = 
    match formula with
    | True -> []
    | False -> []
    | Var x -> [x]
    | Neg f -> formulaToVar f
    | And (f1, f2) -> union (formulaToVar f1) (formulaToVar f2)
    | Or (f1, f2) -> union (formulaToVar f1) (formulaToVar f2)
    | Imply (f1, f2) -> union (formulaToVar f1) (formulaToVar f2)
    | Iff (f1, f2) -> union (formulaToVar f1) (formulaToVar f2)
  in
  let rec substitution formula1 var fbool = 
    match formula1 with
    | True -> True
    | False -> False  
    | Var x -> if x = var then fbool else Var x
    | Neg f -> Neg (substitution f var fbool)
    | And (f1,f2) ->
      And (((substitution f1 var fbool), (substitution f2 var fbool)))
    | Or (f1,f2) ->
      Or (((substitution f1 var fbool), (substitution f2 var fbool)))
    | Imply (f1,f2) ->
      Imply (((substitution f1 var fbool), (substitution f2 var fbool)))
    | Iff (f1,f2) ->
      Iff (((substitution f1 var fbool), (substitution f2 var fbool)))
  in
  let rec calc formula = 
    match formula with
    | Var x -> raise (Failure "cansnot calc variable ")
    | True -> True
    | False -> False
    | Neg x -> if (calc x) != True then True else False
    | And (x,y) -> if (calc x)=True && (calc y)=True then True else False
    | Or (x,y) -> if (calc x)=True || (calc y)=True then  True else False
    | Imply (x,y) -> if (calc x)=True && (calc y)=False then False else True
    | Iff (x,y) -> if  (calc x)=(calc y) then True else False
  in
  let rec satisfierWithVar formula vars =
    match vars with
    | x::xs -> if (satisfierWithVar (substitution formula x True) xs) ||
                  (satisfierWithVar (substitution formula x False) xs)
               then true 
               else false
    | [] -> if (calc formula) = True
            then true
            else false
  in
  satisfierWithVar f (formulaToVar f)
