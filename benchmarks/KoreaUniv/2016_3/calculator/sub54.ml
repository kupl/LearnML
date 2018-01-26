(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented

  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp


   let rec map f l = match l with 
   |[] -> []
   |hd::tl -> (f hd)::(map f tl)


   let rec range: int -> int -> int list = fun n1 n2 ->
   if n1 = n2 then [n1] else n1::(range (n1+1) n2)

   let rec sum: int list -> int = fun l -> match l with
   |[] -> 0
   |hd::tl -> hd + (sum tl)

   let rec evalExp : exp -> int list -> int = fun e env ->
   match e with
   |X -> (match env with
   	|[] -> raise NotImplemented
   	|hd::tl -> hd)
   |INT n -> n
   |ADD(e1, e2) -> (evalExp e1 env) + (evalExp e2 env)
   |SUB(e1,e2) -> (evalExp e1 env)-(evalExp e2 env)
   |MUL(e1,e2) ->(evalExp e1 env)*(evalExp e2 env)
   |DIV(e1,e2) -> (evalExp e1 env)/(evalExp e2 env)
   |SIGMA(e1,e2,e3) -> 
   let n1 = (evalExp e1 env) in
   let n2 = (evalExp e2 env) in
   sum (map (fun n -> evalExp e3 [n]) (range n1 n2))

  let rec calculator : exp -> int = fun exp ->  let env = [] in 
   match exp with
   |X -> (match env with
   	|[] -> raise NotImplemented
   	|hd::tl -> hd)
   |ADD(e1, e2) -> (evalExp e1 env) + (evalExp e2 env)
   |SUB(e1,e2) -> (evalExp e1 env)-(evalExp e2 env)
   |MUL(e1,e2) ->(evalExp e1 env)*(evalExp e2 env)
   |DIV(e1,e2) -> (evalExp e1 env)/(evalExp e2 env)
   |INT n -> n
   |SIGMA(e1,e2,e3) -> 
   let n1 = (evalExp e1 env) in
   let n2 = (evalExp e2 env) in
    sum (map (fun n -> evalExp e3 [n]) (range n1 n2))
