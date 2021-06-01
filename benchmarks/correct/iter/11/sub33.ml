exception InvalidInput		(* Exception : 반복 횟수 n이 음수일 경우 *)

let rec iter (n, f) x =
	if n<0 then raise InvalidInput
	else if n=0 then x
	else f (iter (n-1, f) x)
