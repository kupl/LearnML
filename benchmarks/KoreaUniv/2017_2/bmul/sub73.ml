(* problem 7*)
type digit = ONE | ZERO
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 ->
(* 리스트 길이*)
let rec len a =
 match a with
 |[] -> 0
 |hd::tl -> 1 + len tl

(* 2의 제곱*)
in let rec mul n =
 if n > 0 then 2 * (mul (n-1))
 else 1

(*bin -> int*)
in let rec binary l =
 match l with
 |[] -> 0
 |hd::tl -> if hd = ONE then mul ((len l)-1) + binary tl
            else binary tl

(*int -> bin*)
in let rec bi n =
 if n = 0 then [ZERO]
 else if n = 1 then [ONE]
 else if (n mod 2) = 1 then bi ((n-1)/2) @ [ONE]
 else bi (n/2) @ [ZERO]

in bi ((binary b1) * (binary b2))
;;
