type ae = CONST of int
|VAR of string
|POWER of string * int
|TIMES of ae list
|SUM of ae list

let rec minimize ae =

let rec sum_trim al =
match al with
|[]->[]
|(SUM lst)::tl -> sum_trim (lst@tl)
|hd::tl -> hd::(sum_trim tl)
in

let rec mul_trim al =
match al with
|[]->[]
|(TIMES lst)::tl -> mul_trim (lst@tl)
|hd::tl -> hd::(mul_trim tl)
in

let rec iter lst =
match lst with
|[] -> []
|hd::tl -> (minimize hd)::(iter tl)
in
(*
let rec mul_dist aet =

let rec make_times lst tlst =
match lst with
|[] -> []
|hd::tl -> (TIMES (hd::tlst))::(make_times tl tlst)
in 
let rec iter_mul lst =
match lst with
|[] -> []
|hd::tl -> (mul_dist hd)::(iter_mul tl)
in

match aet with
|TIMES [] -> TIMES []
|TIMES ((SUM lst)::tl) -> SUM (mul_dist (make_times lst tl))
|TIMES (hd::tl) -> 

in
*)

match ae with
|SUM lst -> SUM (iter (sum_trim lst))
(*|TIMES lst -> minimize(mul_dist (TIMES (iter (mul_trim lst)))) *)
|TIMES lst -> TIMES (iter (mul_trim lst))
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
|hd::tl -> (TIMES ((diff (hd, str))::(tl@lst)))::(mul tl (hd::lst))
in
match alex with
|CONST i -> CONST 0
|VAR s -> if (s = str) then CONST 1
	else CONST 0
|POWER (s, i) -> if( s = str) then ( if (i=1) then CONST 1
					else TIMES [CONST i; POWER (s, (i-1))]
						)
		else CONST 0
|TIMES al -> minimize (SUM (mul al []))
|SUM al -> minimize (SUM (sum al))
