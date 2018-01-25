(* problem 1*)
let rec fastexpt : int -> int -> int =
  fun b n -> if n = 0 then 1
  else if (n mod 2) = 0 then let square x = x * x in
    square (fastexpt b (n/2))
  else b * (fastexpt b (n - 1));;

(* problem 2*)
let smallest_divisor : int -> int =
  fun n -> if (n mod 2) = 0 then 2 else let i = 2 in
    let rec compare a b =
      if a >= (b * b) then
        if (a mod b) = 0 then b else compare a (b+1)
      else a in
    compare n i;;

(* problem 3*)
let rec iter : int * (int -> int) -> (int -> int) =
  fun (n,f) -> if n = 0 then (fun x -> x) 
  else let g = iter(n-1, f) in (fun x -> f(g(x)));;

(* problem 4*)
let rec product : (int -> int) -> int -> int -> int =
  fun f a b -> if a = b then f a
  else (f a) * (product f (a + 1) b);;

(* problem 5*)
let rec dfact : int -> int =
  fun n -> if n = 1 then 1
  else if n = 2 then 2
  else n * dfact(n - 2);;

(* problem 6*)
let rec drop : 'a list -> int -> 'a list =
  fun l n -> match l with
  [] -> []
  | hd :: tl -> if n = 0 then l else drop tl (n - 1);;

(* problem 7*)
let rec unzip : ('a * 'b) list -> 'a list * 'b list =
  fun lst -> match lst with
  [] -> ([], [])
  | hd :: tl ->
    (match tl with
    [] -> ([fst hd], [snd hd])
    | _ ->let merge m n = ([fst m] @ fst n, [snd m] @ snd n) in
      merge hd (unzip tl));;

(* problem 8*)
let rec change : int list -> int -> int =
  fun coins amount -> match coins with
  [] -> if (amount = 0) then 1 else 0
  | hd :: tl ->
    if (hd > amount) then (change tl amount)
    else if (amount > 0) then (change coins (amount - hd)) + (change tl amount)
    else if (amount = 0) then 1 else 0;;