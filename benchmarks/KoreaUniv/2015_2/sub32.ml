(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = [];;
let rec filter pred lst = match lst with
  | [] -> []
  | hd :: tl -> if ( pred hd = true ) then [hd] @( filter pred tl ) 
    else filter pred tl;;



(*********************)
(* Problem 2: zipper *)
(*********************)

let rec zipper : int list * int list -> int list
=fun (a,b) -> [] ;;
let rec zipper ((l1 : int list), (l2 : int list)) = match l1 with
  | [] -> l2
  | h1 :: t1 -> match l2 with
                  |[] -> l1
                  |h2 :: t2 -> if (h1 < h2) then h1 :: (zipper (t1, l2))
                              else h2 :: (zipper (l1, t2));;



(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> f;;
let rec iter (n, f ) (x : int) =
  if n < 0 then raise (Failure "n could not be smaller than zero")
  else if n = 0 then x
  else f (iter (n-1, f) x);;

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
=fun (aexp,x) -> match aexp with
    | Const (n) -> Const 0
    | Var (str) -> if (str = x) then Const 1 else Const 0 
    | Power (str, n) -> if (str = x) then Times [Const (n); Power (x, n-1)] else Const 0
    | Times (l) -> (match l with
                | [] -> Const 0
                | hd :: tl -> if (hd = Const 0) then Const 0
                              else Sum ([Times ([diff (hd,x)] @ tl)] @ [Times([hd] @ [diff (Times (tl),x)])]))
    | Sum (l)-> (match l with
              | [] -> Const 0
              | hd :: tl -> if (hd = Const 0) then Sum (tl)
                            else Sum([diff (hd,x) ]@[Sum([diff (Sum (tl),x) ])]));;



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
= fun e -> 0;;
exception FreeVariable;;

let rec calculator exp  = match exp with
  | X -> raise FreeVariable
  | INT (n) -> n
  | ADD (a,b) -> (calculator a) + (calculator b)
  | SUB (a,b) -> (calculator a) - (calculator b) 
  | MUL (a,b) -> (calculator a) * (calculator b) 
  | DIV (a,b) -> (calculator a) / (calculator b) 
  | SIGMA (a,b,f) -> if (calculator a) > (calculator b) then 0
                        else cal (f,a) + calculator (SIGMA (ADD(a,INT 1),b,f))
  and cal (f,a) = match f with
     | X -> calculator a
     | INT n -> n 
     | ADD (f1,f2) -> cal(f1,a) + cal(f2,a)
     | SUB (f1,f2) -> cal(f1,a) - cal(f2,a)
     | MUL  (f1,f2) -> cal(f1,a) * cal(f2,a)
     | DIV  (f1,f2) -> cal(f1,a) / cal(f2,a)
     | SIGMA (c,d,f) -> if (calculator (c) > calculator (d)) then 0 
                        else if (calculator (c) > calculator (d)) then cal (f,c)
                        else (cal (f,c) + calculator ( SIGMA (ADD (INT (calculator c),INT 1),d,f)));;
