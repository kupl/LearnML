(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = 
  match lst with
  |[] -> []
  |hd::tl -> if pred hd then hd::(filter pred tl) else filter pred tl;;

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) ->
  match a with
  |[] -> b
  |hd::tl -> 
   (match b with
    |[] -> a
    |hd2::tl2 -> if hd+0 <hd2+0 then [hd]@[hd2]@(zipper(tl,tl2)) else [hd2]@[hd]@(zipper(tl, tl2)) );;

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> if n=0 then (fun x -> x) else (fun x -> f(iter(n-1, f) x));;

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
|Const n -> Const 0
|Var y -> if y=x then Const 1 else Const 0
|Power(y, n) -> if y=x then (
                    if n=0 then Const 0
                    else if n=1 then Const 1
                    else Times[Const n; Power(y, n-1)])
                else Const 0
|Times l -> 
  (match l with
  |[] -> Const 0
  |hd::[] -> diff(hd,x)
  |hd::tl -> 
    (match hd with
      |Const n -> Times[Const n; diff(Times tl, x)]
      |_ -> Sum[ Times [diff(hd,x); Times tl]; Times [hd; diff(Times tl, x)]  ] ))
|Sum l -> 
  (match l with
  |[]-> Const 0
  |hd::tl -> Sum[diff(hd, x); diff(Sum tl, x)]);;

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

let rec cal_x : exp * exp -> int  (*뒤의 exp에 n을 대입해주는 역할*)
=fun (n, exp) ->
  match exp with
  |X -> cal_x(n, n)
  |INT a -> a
  |ADD(a, b) -> cal_x(n, a) + cal_x(n, b)
  |SUB(a, b) -> cal_x(n, a) - cal_x(n, b)
  |MUL(a, b) -> cal_x(n, a) * cal_x(n, b)
  |DIV(a, b) -> cal_x(n, a) / cal_x(n, b)


let rec sigma : exp * exp * exp -> int
=fun (a, b, f) ->
  if cal_x(a, a) < cal_x(b, b) then cal_x(a, f) + sigma(INT(cal_x(a,a) + 1), b, f)
  else cal_x(a, f)



let rec cal : exp -> int 
=fun exp ->
  match exp with
  |INT n -> n
  |ADD(a, b) -> (cal a) + (cal b)
  |SUB(a, b) -> (cal a) - (cal b)
  |MUL(a, b) -> (cal a) * (cal b)
  |DIV(a, b) -> (cal a) / (cal b)
  |SIGMA(a, b, e) -> 
    (match e with
      |SIGMA(x, y, z) -> (cal x - cal y + 1) * sigma(x, y, z)
      |_ -> sigma(a, b, e)
    )


let calculator : exp -> int
=fun e -> cal e ;;