(* problem 2*)
let is_good_enough guess x = abs_float(guess *. guess -. x) < 0.001;;

let improve guess x = (guess +. x /. guess) /. 2.0;;

let rec sqrt_iter guess x = if (is_good_enough guess x) then guess else sqrt_iter (improve guess x) x;;

let sqrt x = sqrt_iter 1.0 x;;

let rec proc n m = if (n > int_of_float(sqrt (float_of_int(m)) +. 1.0)) then m else (if (m mod n) = 0 then n else proc (n + 1) m);;


let smallest_divisor : int -> int
= fun n -> proc 2 n;;