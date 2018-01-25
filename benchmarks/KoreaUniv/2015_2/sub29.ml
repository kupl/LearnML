(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = 
	match lst with
	[]->[]
	|h::t->if pred h then h::filter pred t else filter pred t

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> 
	match a with
	[]->b
	|h1::t1-> (match b with
		[]->a
		|h2::t2->h1 :: h2 :: zipper (t1,t2))

(*******************)
(* Problem 3: iter *)
(*******************)
let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> 
match n with
0-> fun x->x
|_ -> fun x->iter (n-1,f) (f x)


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
Const n -> Const 0
|Var v -> if v=x then Const 1 else Const 0
|Power (v,n) -> 
  if v = x 
  then 
    if n=1 then Var v
    else Times[Const n; Power ("x",n-1)]
  else Const 0
|Times l -> 
  (
    match l with
    []->Times []
    |h::t -> 
    (match h,t with 
      _,[] -> diff (h,x)
      |Const 0,_->Const 0
      |_,_ -> Sum ( Times (diff (h,x) :: t) :: (Times ( h :: ( diff (Times t,x)::[]) ) ::[])  )
    )
  )
| Sum l ->
(
    match l with 
    []->Const 0
    |h::t -> if t = [] then diff (h,x) else Sum (diff (h,x) :: diff (Sum t,x) ::[]) 
)

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

let rec calculator : exp -> int
=fun e ->
match e with  
X->raise (Failure "error")
|INT n->n
|ADD (e1,e2) -> calculator e1+calculator e2
|SUB (e1,e2)-> calculator e1-calculator e2
|MUL (e1,e2)->calculator e1*calculator e2
|DIV (e1,e2)->calculator e1/calculator e2
|SIGMA (e1,e2,e3) ->
( let a= calculator e1 in
  let b= calculator e2 in
  if a>b then 0 else 
    (match e3 with
      X->a
      |INT n->n
      |ADD (e4,e5) -> calculator (SIGMA(e1,e1,e4))+calculator (SIGMA(e1,e1,e5))
      |SUB (e4,e5)-> calculator (SIGMA(e1,e1,e4))-calculator (SIGMA(e1,e1,e5))
      |MUL (e4,e5)-> calculator (SIGMA(e1,e1,e4))*calculator (SIGMA(e1,e1,e5))
      |DIV (e4,e5)-> calculator (SIGMA(e1,e1,e4))/calculator (SIGMA(e1,e1,e5))
      |SIGMA (_,_,_) -> raise (Failure "error") (*시그마의 e3자리에 또 시그마가 있는건 에러*)
    )+calculator (SIGMA(INT (a+1),INT b,e3) )
)

