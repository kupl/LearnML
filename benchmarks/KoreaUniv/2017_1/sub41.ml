(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n ->
    if n == 0 then 1
    else match n mod 2 with
    | 0 -> fastexpt (b * b) (n / 2)
    | _ -> b * (fastexpt b (n - 1))

(* problem 2*)

let smallest_divisor : int -> int
= fun n ->
    if n mod 2 == 0 then 2
    else let rec loop r d =
        if n mod d == 0 then d
        else
            if d >= r then 0
            else loop r (d + 2)
        in
            let result =
                loop (int_of_float (sqrt (float_of_int n))) 3 in
                if result == 0 then n
                else result

(* problem 3*)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->
    if n == 0 then let id x = x in id
    else let compose f g = fun x -> f (g x) in
        compose f (iter (n - 1, f))

(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b ->
    if a > b then 1
    else (product f (a + 1) b) * (f a)

(* problem 5*)

let dfact : int -> int
= fun n ->
    if n mod 2 == 0 then product (fun x -> 2 * x) 1 (n / 2)
    else product (fun x -> 2 * x - 1) 1 ((n + 1) / 2)

(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n ->
    if n <= 0 then l
    else match l with
    | _ :: tail -> drop tail (n - 1)
    | _ -> l

(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
    let merge (x1, y1) (x2, y2) = (x1 :: x2, y1 :: y2) in
    match lst with
    | (a, b) :: tail -> merge (a, b) (unzip tail)
    | _ -> ([], [])

(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount ->
    if amount == 0 then 1
    else if amount < 0 then 0
    else match coins with
    | c :: rest -> let rec loop = fun n ->
        if n < 0 then 0
        else change rest (amount - n * c) + loop (n - 1)
    in loop (amount / c)
    | _ -> 0
