(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = 
  match lst with
    [] -> []
   |hd::tl -> if (pred hd) then hd::(filter pred tl) else (filter pred tl)

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> 
  match a with
    [] -> if b = [] then [] else zipper(b, [])
   |hd::tl -> hd::zipper(b,tl)

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> 
  if n < 0 then raise (Failure "n cannot be the neg number")
  else
    (match n with
      0 -> fun x->x 
     |1 -> f
     |_ -> iter(n-1, fun x -> f (f x) ) ) 

(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec iscontainconstzero 
=fun list -> 
  match list with
    [] -> false
   |hd::tl -> if hd = Const 0 then true else iscontainconstzero tl

exception InvalidArgument
let rec diff : aexp * string -> aexp
=fun (aexp,x) -> 
  match aexp with 
    Const n -> Const 0
   |Var v -> 
       if v = x then Const 1 else Const 0 
   |Power (p, n) ->
       if p = x then
          (match n with
               0 -> Const 0
              |1 -> Const 1
              |_ -> Times [Const n; Power(p, n-1)]
          )
       else Const 0
   |Times t ->
       if iscontainconstzero t then Const 0
       else 
            (match t with 
              [] -> raise InvalidArgument
             |hd::[] -> diff (hd, x)
             |hd::tl -> 
                 (match hd with
                     Const 1 -> diff(Times tl, x)
                    |Const n -> Times [hd; diff(Times tl, x)]
                    |_ -> Sum [Times (diff(hd, x)::tl); Times [hd; diff(Times tl, x)]] 
                 )
            )

   |Sum s ->
       match s with
           [] -> raise InvalidArgument
          |hd::[] -> diff (hd, x)
          |hd::tl -> Sum[ diff(hd, x); diff(Sum tl, x); ]

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

exception DivideByZero

let rec eval
=fun e ->
	match e with 
		X -> raise InvalidArgument
		| INT i -> i
		| ADD (e1, e2) -> 
			let r1 = eval e1 in
			let r2 = eval e2 in
				r1 + r2
		| SUB (e1, e2) -> 
			let r1 = eval e1 in
			let r2 = eval e2 in
				r1 - r2
		| MUL (e1, e2) ->  
			let r1 = eval e1 in
			let r2 = eval e2 in
				r1 * r2
		| DIV (e1, e2) -> 
			let r1 = eval e1 in
			let r2 = eval e2 in
				if r2 = 0 then raise DivideByZero else r1 / r2
		| SIGMA (sc, tc, e) -> 
			let sc' = eval sc in
			let tc' = eval tc in
			if sc' > tc' then 0 
			else 
			(
				eval (SIGMA (INT (sc'+1), INT tc', e)) + (sigma e sc')
			)

and sigma
=fun e i ->
	match e with
		X -> i
		| INT n -> n
		| ADD (e1, e2) -> eval (ADD (INT (eval (SIGMA (INT i, INT i, e1))) ,INT (eval (SIGMA (INT i, INT i, e2)))))
		| SUB (e1, e2) -> eval (SUB (INT (eval (SIGMA (INT i, INT i, e1))) ,INT (eval (SIGMA (INT i, INT i, e2)))))
		| MUL (e1, e2) -> eval (MUL (INT (eval (SIGMA (INT i, INT i, e1))) ,INT (eval (SIGMA (INT i, INT i, e2)))))
		| DIV (e1, e2) -> eval (DIV (INT (eval (SIGMA (INT i, INT i, e1))) ,INT (eval (SIGMA (INT i, INT i, e2)))))
		| SIGMA (sc, tc, e') -> eval (SIGMA (sc, tc, INT (eval (SIGMA (INT i, INT i, e')))))

let calculator : exp -> int
=fun e -> eval e
