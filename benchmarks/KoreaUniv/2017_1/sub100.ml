(* problem 1*)

let square x = x*x

let rec fastexpt : int -> int -> int
= fun b n ->
    match n with
    | 0 -> 1
    | _ -> if n mod 2 = 0 then square (fastexpt b (n/2))
           else b * fastexpt b (n-1)

(* problem 2*)

let rec smallest_divisor2 n p =
  if (square p) > n then n
  else if n mod p = 0 then p
  else smallest_divisor2 n (p+1)

let smallest_divisor : int -> int
= fun n -> smallest_divisor2 n 2

(* problem 3*)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->
    if n = 0 then fun x -> x
    else fun x -> (f (iter ((n-1), f) x))

(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b ->
    if a = b then f a
    else (f a) * (product f (a+1) b)

(* problem 5*)

let dfact : int -> int
= fun n ->
    if n mod 2 = 0 then product (fun x -> 2*x) 1 (n/2)
    else product (fun x -> 2*x-1) 1 ((n+1)/2)

(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n ->
    if n = 0 then l
    else match l with
         | [] -> []
         | hd::tl -> drop tl (n-1)

(* problem 7*)

let rec lappend l n =
  match l with
  | [] -> n::[]
  | hd::tl -> hd::(lappend tl n)

let rec unzip2 l out1 out2 =
  match l with
  | [] -> (out1, out2)
  | (f, s)::tl -> unzip2 tl (lappend out1 f) (lappend out2 s)

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> unzip2 lst [] []

(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount ->
    match coins with
    | [] -> if amount = 0 then 1
            else 0
    | hd::tl -> if hd > amount then change tl amount
                else (change coins (amount-hd)) + (change tl amount)
