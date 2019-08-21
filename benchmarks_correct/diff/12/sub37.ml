type aexp = Const of int
|Var of string
|Power of string * int
|Times of aexp list
|Sum of aexp list

let rec minimize aexp =

let rec sum_trim al =
match al with
|[]->[]
|(Sum lst)::tl -> sum_trim (lst@tl)
|hd::tl -> hd::(sum_trim tl)
in

let rec mul_trim al =
match al with
|[]->[]
|(Times lst)::tl -> mul_trim (lst@tl)
|hd::tl -> hd::(mul_trim tl)
in

let rec iter lst =
match lst with
|[] -> []
|hd::tl -> (minimize hd)::(iter tl)
in
(*
let rec mul_dist aexpt =

let rec make_times lst tlst =
match lst with
|[] -> []
|hd::tl -> (Times (hd::tlst))::(make_times tl tlst)
in 
let rec iter_mul lst =
match lst with
|[] -> []
|hd::tl -> (mul_dist hd)::(iter_mul tl)
in

match aexpt with
|Times [] -> Times []
|Times ((Sum lst)::tl) -> Sum (mul_dist (make_times lst tl))
|Times (hd::tl) -> 

in
*)

match aexp with
|Sum lst -> Sum (iter (sum_trim lst))
(*|Times lst -> minimize(mul_dist (Times (iter (mul_trim lst)))) *)
|Times lst -> Times (iter (mul_trim lst))
|x -> x


let rec diff (alex, str) =
let rec sum al =
match al with
|[] -> []
|hd::tl -> (diff (hd, str))::(sum tl)
in
let rec mul al lst =
match al with
|[] -> []
|hd::tl -> (Times ((diff (hd, str))::(tl@lst)))::(mul tl (hd::lst))
in
match alex with
|Const i -> Const 0
|Var s -> if (s = str) then Const 1
	else Const 0
|Power (s, i) -> if( s = str) then ( if (i=1) then Const 1
					else Times [Const i; Power (s, (i-1))]
						)
		else Const 0
|Times al -> minimize (Sum (mul al []))
|Sum al -> minimize (Sum (sum al))
