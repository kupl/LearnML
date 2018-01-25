(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n ->
	if n <= 0 then (* last step *)
		1
	else if n == 1 then (* b powered by 1 *)
		b
	else if (n mod 2) == 1 then (* odd number *)
		b * (fastexpt b (n - 1))
	else (* even number *)
		(fastexpt b (n / 2)) * (fastexpt b (n / 2))
;;