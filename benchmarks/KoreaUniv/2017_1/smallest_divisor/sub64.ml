(* problem 2*)

let smallest_divisor : int -> int
= fun n ->
	if n mod 2 == 0 then 2
	else
	(
		let rec div
		= fun sqr n cur ->
		(
			if sqr < cur then n (* if there is no divisor smaller than (sqrt n), return original n *)
			else if n mod cur == 0 then cur (* if n is divisible by cur, return cur *)
			else div sqr n (cur+1)
		)
		in div (int_of_float (sqrt (float_of_int n))) n 2
	)
;;