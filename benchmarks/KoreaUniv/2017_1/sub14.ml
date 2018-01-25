(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n -> if (n = 0) then 1 else (if (n mod 2) = 0 then fastexpt (b * b) (n / 2) else b * fastexpt b (n - 1));;



(* problem 2*)
let is_good_enough guess x = abs_float(guess *. guess -. x) < 0.001;;

let improve guess x = (guess +. x /. guess) /. 2.0;;

let rec sqrt_iter guess x = if (is_good_enough guess x) then guess else sqrt_iter (improve guess x) x;;

let sqrt x = sqrt_iter 1.0 x;;

let rec proc n m = if (n > int_of_float(sqrt (float_of_int(m)) +. 1.0)) then m else (if (m mod n) = 0 then n else proc (n + 1) m);;


let smallest_divisor : int -> int
= fun n -> proc 2 n;;



(* problem 3*)
let fst (x, _) = x;;

let snd (_, x) = x;;

let compose f g = fun x -> f(g(x));;

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> if (n = 0) then (fun x -> 1 * x) else (compose (f) (iter (n - 1, f)));;



(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b -> if (a > b) then 1 else (f a) * (product f (a + 1) b);;



(* problem 5*)

let dfact : int -> int
= fun n -> if (n mod 2) = 0 then (product (fun x -> 2 * x) 1 (n / 2)) else (product (fun x -> 2 * x - 1) 1 ((n + 1) / 2));;



(* problem 6*)
let rec drop : 'a list -> int -> 'a list
= fun l n -> if (n <= 0) then l else (match l with |[] -> []|hd::tl -> drop tl (n - 1));;



(* problem 7*)
let rec proc2 f l = match l with |[] -> []|hd::tl -> (f hd)::(proc2 f tl);;

let combine l1 l2 = (l1, l2);;

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> combine (proc2 fst lst) (proc2 snd lst);;



(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount -> if (amount = 0) then 1 else (if (amount < 0) then 0 else match coins with |[] -> 0 |hd::tl -> (change coins (amount - hd)) + (change tl amount));;

