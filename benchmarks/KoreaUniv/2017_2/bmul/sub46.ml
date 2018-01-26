(* problem 7*)
type digit = ZERO | ONE
type bin = digit list


let rec reverse l = match l with |[] -> []| hd::tl -> (reverse tl) @ [hd]

let rec to_dec b n = match b with |[] -> 0|hd::tl -> if (hd=ZERO) then (to_dec (tl) (2 * n)) else (1 * n) + (to_dec (tl) (2 * n))

let proc b = to_dec (reverse b) 1

let rec to_bin n = match n with |0 -> [ZERO] |1 -> [ONE] |_->(to_bin (n / 2)) @ (to_bin (n mod 2))

let bmul : bin -> bin -> bin
= fun b1 b2 -> to_bin ((proc b1) * (proc b2))
