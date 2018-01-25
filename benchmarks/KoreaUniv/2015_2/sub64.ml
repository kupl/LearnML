(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter : ('a -> bool) -> 'a list -> 'a list 
=fun pred lst -> 
  if (List.length lst) > 0 then
    if (pred (List.hd lst)) then (List.hd lst)::(filter pred (List.tl lst))
    else filter pred (List.tl lst)
  else [] 

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> 
  match (a,b) with
  | ([], []) -> []
  | ([], _) -> b
  | (_, []) -> a
  | _ -> (List.hd a)::(List.hd b)::zipper((List.tl a), (List.tl b)) 

(*******************)
(* Problem 3: iter *)
(*******************)
let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> 
  match (n,f) with
  | (0, _) -> fun x -> x 
  | (1, _) -> f
  | _ -> fun x -> iter(n-1, f) (f x)

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
  match aexp with
  | Const _ -> Const 0
  | Var (a) -> 
    if a = x then Const 1
    else Const 0
  | Power (a, n) ->
    (match (a, n) with 
     | ("x", 0) -> Const 0
     | ("x", 1) -> Const n
     | ("x", _) -> Times [Const n; Power(a, n-1)]
     | _ -> Const 0)
  | Times (lst) ->  
    if (List.length lst) = 0 then Const 0
    else if (List.length lst) = 1 then diff (List.hd lst, x)
    else Times [List.hd lst; diff (Sum (List.tl lst), x)]
  | Sum (sum) ->
    if (List.length sum) = 0 then Const 0
    else if (List.length sum) = 1 then diff(List.hd sum, x) 
    else Sum [ diff(List.hd sum, x) ; diff (Sum (List.tl sum),x)]

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
=fun e ->
  let rec sigma f l func = 
      if f <= l then (func f) + (sigma (f+1) l func) 
      else 0 
  in let rec exp_to_func : exp -> (int -> int) 
  =fun input ->
    match input with
    | X -> (fun a -> a)
    | INT (i) -> (fun a -> i) 
    | ADD (x, y) -> (fun a -> ((exp_to_func x) a) + ((exp_to_func y) a))  
    | SUB (x, y) -> (fun a -> ((exp_to_func x) a) - ((exp_to_func y) a))
    | MUL (x, y) -> (fun a -> ((exp_to_func x) a) * ((exp_to_func y) a))
    | DIV (x, y) -> (fun a -> ((exp_to_func x) a) / ((exp_to_func y) a))
  in match e with
  | INT (i) -> i 
  | ADD (INT x, INT y) -> x + y   
  | SUB (INT x, INT y) -> x - y 
  | MUL (INT x, INT y) -> x * y 
  | DIV (INT x, INT y) -> x / y 
  | SIGMA (INT (x), INT (y), z) -> sigma x y (exp_to_func z)
