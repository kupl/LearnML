(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = 
  match lst with
    [] -> []
  | head :: [] -> if pred head = true then lst
                  else []
  | head :: tail -> 
    if pred head = true then head::(filter pred tail)
    else filter pred tail


(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> 
  match a, b with
    [], [] -> []
  | a, [] -> a
  | [], b -> b
  | ahead::atail, bhead::btail -> ahead::bhead::zipper(atail, btail)



(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> 
  if n = 0 then fun x -> x
  else fun x -> f (iter (n-1, f) x)




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
=fun (aexp,x) ->
  match aexp, x with
    Const c, _ -> Const 0
  | Var a, t -> if a = t then Const 1    (* f(x)d/dx = f'(x) *)
                else Const 0             (* f(x)d/dt = 0 *)
  | Power (a, b), t -> if a = t then Times [Const b; Power (a, b-1)]
                       else Const 0
  | Times lst, t -> (match lst with
                      [] -> Const 0
                    | head::[] -> diff (head, t)
                    | head::tail -> Sum [(Times ([diff (head, t)]@ tail)); (Times [head; diff (Times tail, t)])])
  | Sum lst, t -> (match lst with
                    [] -> Const 0
                  | head::[] -> diff (head, t)
                  | head::tail -> Sum ([diff (head, t)]@ [diff (Sum tail, t)]))
                  



 
(*************************)
(* Problem 5: Calculator *)
(*************************)
type exp =(* X*)
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
        (* | SIGMA of exp * exp * exp*)

(*let rec sigma : int * int * (int -> int) -> int
=fun (a, b, f) -> if a = b then f (a)
                  else ((f (b) + sigma (a, b-1, f))) *)


let rec calculator : exp -> int    
=fun e -> match e with 
           (*X -> calculator X*)
          | INT a -> a
          | ADD (a, b) -> calculator a + calculator b
          | SUB (a, b) -> calculator a - calculator b
          | MUL (a, b) -> calculator a * calculator b
          | DIV (a, b) -> calculator a / calculator b
        (*  | SIGMA (a, b, f) -> sigma (calculator a, calculator b, calculator f) *)

