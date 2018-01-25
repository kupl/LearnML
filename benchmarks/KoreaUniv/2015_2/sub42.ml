(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = 
  match lst with
  | [] -> []
  | hd :: tl -> if (pred hd) then hd :: (filter pred tl) else (filter pred tl);;






(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> 
  match b with
  | [] -> a
  | hd :: tl ->
      (match a with
      |[] -> b
      | hda :: tla -> if hd<hda then (zipper(hd::a, tl)) else ( hda::zipper(tla , b) ));;




(*******************)
(* Problem 3: iter *)
(*******************)
let id = fun x -> x
let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) id->
  match n with
  |0 -> id
  |_ -> f (iter(n-1,f) id);;



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
  |Power(a,b) ->Times[Const b ; Var a] 
  |Const n ->Const 0
  |Var n -> Var n
  |Times (hd :: tl) -> Sum [ (Times( diff(hd,x) :: tl ) ) ; (Times( hd :: [diff(Times(tl),x)] ) ) ]
  |Times [] ->Const 0
  |Sum (hd :: tl) -> Sum[ diff(hd,x) ; diff(Sum(tl),x)]
  |Sum [] -> Const 0;;



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
    let rec cal : exp-> int
    = fun e ->(
    match e with
    |X -> 0
    |INT n -> n
    |ADD (a,b) -> (cal a)+(cal b)
    |SUB (a,b) -> (cal a)-(cal b)
    |MUL (a,b) -> (cal a)*(cal b)
    |DIV (a,b) -> (cal a)/(cal b)
    |SIGMA (a,b,c) -> 
      (let rec func : exp * int -> int
      = fun (n,i) -> match n with
       |X-> i
       |INT n -> n
       |ADD (x,y) -> (func(x,i)) + (func(y,i))
       |SUB (x,y) -> func(x,i) - func(y,i)
       |MUL (x,y) -> func(x,i) * func(y,i)
       |DIV (x,y) -> func(x,i) / func(y,i)
       |_ ->i
      in
       if a<=b then cal( ADD( INT(func (c , (cal a))) , SIGMA( INT( cal(a) + 1 ), b, c ) ) ) else 0) )
  in
  cal e;;



