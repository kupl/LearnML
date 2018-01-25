
(*********************) 
(* Problem 1: filter *) 
(*********************) 
let rec filter pred lst =  (* TODO *) 
 
match lst with 
|[] -> [] 
|hd::tl -> if (pred hd) == true then [hd]@ (filter pred tl) else (filter pred tl);; 
 
(*********************) 
(* Problem 2: zipper *) 
(*********************) 
let rec zipper : int list * int list -> int list 
=fun (a,b) ->  (* TODO *) 
 
match a, b with 
    |[],_ -> b 
    |_,[] -> a 
    |hd1::tl1, hd2::tl2 -> [hd1] @ [hd2] @ (zipper (tl1, tl2));; 
 
 
(*******************) 
(* Problem 3: iter *) 
(*******************) 
 
let rec iter : int * (int -> int) -> (int -> int) 
=fun (n,f) -> f (* TODO *) 
 
(*********************) 
(* Problem 4: Diff   *) 
(*********************) 
type aexp =  
  | Const of int  
  | Var of string  
  | Power of string * int  
  | Times of aexp list 
  | Sum of aexp list 
 
let rec diff : aexp * string -> aexp 
=fun (aexp,x) -> aexp (* TODO *) 
 
(*************************) 
(* Problem 5: Calculator *) 
(*************************) 
type exp = X 
         | INT of int 
         | ADD of exp * exp 
         | SUB of exp * exp 
         | MUL of exp * exp 
         | DIV of exp * exp 
         | SIGMA of exp * exp * exp 
let calculator : exp -> int 
=fun e ->  (* TODO *) 
    let exptoint exp = 
        match exp with 
        INT n -> n in 
            match e with 
            ADD(e1,e2) -> exptoint e1 + exptoint e2 
            |SUB(e1,e2) -> exptoint e1 - exptoint e2 
            |MUL(e1,e2) -> exptoint e1 * exptoint e2 
            |DIV(e1,e2) -> exptoint e1 / exptoint e2;; 