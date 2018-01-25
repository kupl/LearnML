(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n ->
    if n = 0 then 1
    else
      let b2 = fastexpt b (n / 2) in
        if (n mod 2) = 0 then b2 * b2
        else b * b2 * b2;;

(* problem 2*)

let smallest_divisor : int -> int
= fun n ->


(* problem 3*)

let iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> (*TODO*)

(* problem 4*)

let product : (int -> int) -> int -> int -> int
= fun f a b -> (*TODO*)

(* problem 5*)

let dfact : int -> int
= fun n -> (*TODO*)

(* problem 6*)

let drop : 'a list -> int -> 'a list
= fun l n -> (*TODO*)

(* problem 7*)

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> (*TODO*)

(* problem 8*)

let change : int list -> int -> int
= fun coins amount -> (*TODO*)
