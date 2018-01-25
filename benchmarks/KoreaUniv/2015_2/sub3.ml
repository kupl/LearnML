(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter func list =
  match list with
  |[] -> []
  |hd::tl -> if func hd = true then hd::(filter func tl) else filter func tl

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (l1, l2) ->
  match l1 with
  |[] -> []
  |hd::tl -> if tl = [] then hd::l2 else hd::(zipper (l2, tl))

(*******************)
(* Problem 3: iter *)
(*******************)
let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> fun x -> 
  if n = 0 then x  
  else iter(n-1, f) (f x)

(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff_times(lst, x, b)
= match lst with
  | [] -> Const 0
  | hd::tl -> (match hd with
     | Const a -> diff_times (tl, x, a+b)
     | Var a -> if a = x then Const b else diff_times (tl, x, b)
     | Power (a, k) -> if a = x then
       (if k > 1 then Times [Const (b*k); Power(a, k-1)]
	   else Const (b*k))
       else Const 0
     | _ -> diff_times (tl, x, b))

let rec diff(aexp, x) =
match aexp with
  | Sum lst -> Sum (match lst with
    | [] -> []
    | hd::tl -> diff(hd, x)::[diff(Sum tl, x)])
  | Times lst -> diff_times(lst, x, 0)
  | Var a -> if a = x then Const 1 else Const 0
  | Const a -> Const 0
  | Power (a, b) -> if b > 1 then Times[Const b; Power (a, b-1)] else Const b

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

let rec sigma_fun(a, b, c) = 
if a < b then (match c with
  | X -> a
  | INT q -> q
  | ADD (q, w) -> sigma_fun(a, a, q) + sigma_fun(a, a, w) + sigma_fun(a+1, b, c)
  | SUB (q, w) -> sigma_fun(a, a, q) - sigma_fun(a, a, w) + sigma_fun(a+1, b, c)
  | MUL (q, w) -> sigma_fun(a, a, q) * sigma_fun(a, a, w) + sigma_fun(a+1, b, c)
  | DIV (q, w) -> sigma_fun(a, a, q) / sigma_fun(a, a, w) + sigma_fun(a+1, b, c)
  | SIGMA(INT q, INT w, e) -> sigma_fun(q, w, e) + sigma_fun(a+1, b, c)
  | _ -> 0)
else if a = b then(match c with
  | X -> a 
  | INT q -> q
  | ADD (q, w) -> sigma_fun(a, a, q) + sigma_fun(a, a, w)
  | SUB (q, w) -> sigma_fun(a, a, q) - sigma_fun(a, a, w)
  | MUL (q, w) -> sigma_fun(a, a, q) * sigma_fun(a, a, w)
  | DIV (q, w) -> sigma_fun(a, a, q) / sigma_fun(a, a, w)
  | SIGMA(INT q, INT w, e) -> sigma_fun(q, w, e)
  | _ -> 0)
else 0

let calculator : exp -> int
=fun e -> match e with
  | INT a -> a
  | ADD (INT a, INT b) -> a + b
  | SUB (INT a, INT b) -> a - b
  | MUL (INT a, INT b) -> a * b
  | DIV (INT a, INT b) -> a / b
  | SIGMA (INT a, INT b, c) -> sigma_fun(a, b, c)
  | _ -> 0