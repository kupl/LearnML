(* problem 2*)

let rec oddmod n odd nsqrt =
	if odd<=nsqrt then (if n mod odd = 0 then odd
								else (oddmod n (odd+2) nsqrt))
	else n;;

let rec smallest_divisor : int -> int
= fun n ->
	let nsqrt = int_of_float(sqrt(float_of_int(n))) in
	if n mod 2 = 0 then 2(*짝수일때*)
	else oddmod n 3 nsqrt;;


	(*홀수일때 sqrt해주고 3부터 시작하는 홀수이자 소수로 나눠지는 것 찾기 안나눠지면 입력된 n이 smallest_divisor*)