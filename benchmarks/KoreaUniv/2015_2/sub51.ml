(* Problem 1: filter *)
let rec filter pred lst= 
 match lst with
  |[] -> []
  |h::t -> if pred h = true then h::(filter pred t)
           else (filter pred t);;

(* Problem 2: zipper *)
let rec zipper : int list * int list -> int list
=fun (a,b) -> match a,b with
 |[],b -> b
 |a,[] -> a
 |h1::t1,h2::t2 -> if h1<h2 then h1::(zipper (t1, b))
              else h2::(zipper (a, t2));;


(* Problem 3: iter *)
let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> match n with
|0 -> f
|_ -> iter(n-1,f)

(* Problem 4: Diff   *)
type aexp =
| Const of int
| Var of string
| Power of string * int
| Times of aexp list
| Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) -> match aexp with
|Const a -> Const 0
|Var "x" -> Const 1
|Power ("x", a) -> (match a with
 |2 -> Times[Const 2; Var "x"]
 |1 -> Const 1
 |0 -> Const 0
 |_ -> Times[Const a; Power ("x", a-1)])

(*
|Times l -> (match l with
 |h::t -> (match t with
  |Var "x" -> h
  |Power ("x",a) -> Times [Times [h;Const a];diff(Power ("x",a),x)])))
|Sum l -> (match l with
 |h::t -> Sum (diff(h,x)::diff(t,x)))
*)

(*Times에서 리스트에 들어가는 요소의 타입을 계속 찾지 못 했습니다*)

(* Problem 5: Calculator *)
type exp =
| INT of int
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp

let calculator : exp -> int
=fun e -> match e with
|INT x -> x
|ADD(INT x,INT y) -> x+y
|SUB(INT x,INT y) -> x-y
|MUL(INT x,INT y) -> x*y
|DIV(INT x,INT y) -> x/y
|SIGMA(INT x, INT y,e3) -> if x=y then (calculator e3)
 else (calculator e3) + (calculator (SIGMA(INT x,INT (y-1),e3)));;