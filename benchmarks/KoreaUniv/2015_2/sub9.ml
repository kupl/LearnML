
(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = match lst with
[] -> lst |
h::t -> if pred h then h::filter pred t else filter pred t
(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> match a with
[] -> b |
ha::ta -> ha::match b with
  [] -> zipper (ta,[]) |
  hb::tb -> hb:: zipper (ta, tb)

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> match n with
0 -> (fun x -> x) |
1 -> f |
_ -> (fun x -> iter(n-1,f) (f x))

(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list;;

let rec check_times : aexp list * string -> int
=fun (li,str) -> match li with
[] ->  0|
h::t -> (match h with 
  Var str3 ->  if str=str3 then 1 else check_times (t,str) |
  Power (str2,a) -> if str=str2 then 2 else check_times (t,str) |
  _ -> check_times (t,str));;

let rec align_head : aexp * string -> aexp
=fun (aexp, x) -> match aexp with
Times (h::t) -> if h=Var x then Times (h::t) else align_head (Times (t@[h]), x)
| _ -> raise (Failure "HowDidYouGetHere");;

let work_head : aexp * string -> aexp
=fun (aexp, x) -> match aexp with
Times (h::t) -> Times t
| _ -> raise (Failure "HowDidYouGetHere");;

let rec align_head_power : aexp * string -> aexp
=fun (aexp, x) -> match aexp with
Times (h::t) -> (match h with
  Power(x,_) -> Times (h::t)|
  _ -> align_head_power (Times (t@[h]), x))
| _ -> raise (Failure "HowDidYouGetHere");;

let rec diff : aexp * string -> aexp
=fun (aexp,x) -> match aexp with
Sum (h::t) -> Sum [diff(h,x);diff(Sum t,x)]|
Times li -> (match check_times (li,x) with
  0 -> Const 0|
  1 -> work_head (align_head (Times li,x),x)|
  2 -> work_head_power ((align_head_power (Times li,x)),x)|
  _ -> Const 0)|
Sum [] -> Const 0|
Const a -> Const 0|
Var str -> if str=x then Const 1 else Const 0|
Power (str,a) -> if str=x then Times [Const a; Power (str,a-1)] else Const 0

and work_head_power : aexp * string -> aexp
=fun (aexp, x) -> match aexp with
Times (h::t) -> Times ([diff (h,x)]@t)
| _ -> raise (Failure "HowDidYouGetHere");;

type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp;;

let rec calculator : exp -> int
=fun e -> (match e with
INT a -> a |
ADD (a,b) -> calculator a + calculator b |
SUB (a,b) -> calculator a - calculator b |
MUL (a,b) -> calculator a * calculator b |
DIV (a,b) -> calculator a / calculator b |
SIGMA (a,b,c) -> if a=b then calculator(foo(c,a)) else
  (calculator(foo(c,a)) + calculator(SIGMA(INT(calculator(ADD(a,INT 1))),b,c))))

and foo : exp * exp -> exp
=fun (c,x) -> match c with
INT a -> INT a |
ADD(a,b) -> ADD(foo(a,x),foo(b,x)) |
SUB(a,b) -> SUB(foo(a,x),foo(b,x)) |
MUL(a,b) -> MUL(foo(a,x),foo(b,x)) |
DIV(a,b) -> DIV(foo(a,x),foo(b,x)) |
X -> x |
SIGMA (a,b,c) -> SIGMA(a,b,c);;