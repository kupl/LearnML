(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n -> if n=0 then 1 else if n mod 2 = 0 then (fastexpt b (n/2)) * (fastexpt b (n/2)) else b * (fastexpt b (n-1));;

(* problem 2*)
let smallest_divisor : int -> int
= fun n -> let rec div n m i = if m<i then n else if n mod i = 0 then i else (div n m (i+1)) in div n (int_of_float(sqrt(float_of_int n))) 2;;

(* problem 4*)
let rec product : (int -> int) -> int -> int -> int
= fun f a b -> if b>=a then (f a) * (product f (a+1) b) else 1;;

(* problem 5*)
let rec dfact : int -> int
= fun n -> if n<=0 then 1 else n*dfact(n-2);;

(* problem 6*)
let rec drop : 'a list -> int -> 'a list
= fun l n -> if n=0 then l else match l with []->[] | hd::tl -> drop tl (n-1);;
