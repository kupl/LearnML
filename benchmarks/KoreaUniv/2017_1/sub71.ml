(* problem 1*)

let rec fastexpt : int -> int -> int
= fun b n ->
  if n = 0 then 1
  else if n < 0 then raise (Failure ("ValueError: n must be a positive integer."))
  else if (n mod 2) <> 0 then b * (fastexpt b (n-1))
       else let re = (fastexpt b (n/2)) in re * re

(* problem 2*)

let rec div_helper : int -> int -> int -> int
= fun n div limit ->
  if (n mod div) = 0 then div
  else if div >= limit then n
       else div_helper n (div+2) limit

let smallest_divisor : int -> int
= fun n ->
  if (n mod 2) = 0 then 2
  else let limit = (int_of_float (sqrt (float_of_int n))) in
       let result = (div_helper n 3 limit) in
       result

(* problem 3*)

let compose : (int -> int) -> (int -> int) -> (int -> int)
= fun f g -> (fun n -> f(g(n)))

let rec iter_helper : int -> (int -> int) -> (int -> int) -> (int -> int)
= fun n prev f ->
  if n = 0 then prev
  else iter_helper (n-1) (compose prev f) f

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->
  if n = 0 then (fun x -> x)
  else if n < 0 then raise (Failure ("ValueError: n must be a non-negative integer."))
  else iter_helper (n-1) f f

(* problem 4*)

let rec gen_list : int -> int -> (int list) -> (int list)
= fun n limit l ->
  if n > limit then l
  else gen_list (n+1) limit (l @ [n])

let rec fold f l a =
  match l with
  | [] -> a
  | hd::tl -> f hd (fold f tl a)

let product : (int -> int) -> int -> int -> int
= fun f a b ->
  let l = gen_list a b [] in
  fold (fun x y -> (f x) * y) l 1

(* problem 5*)

let dfact : int -> int
= fun n ->
  if n < 0 then raise (Failure ("ValueError: n must be a non-negative integer."))
  else if (n mod 2) = 0 then product (fun x -> 2*x) 1 (n/2)
  else product (fun x -> 2*x-1) 1 ((n+1)/2)

(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n ->
  if n < 0 then raise (Failure ("ValueError: n must be a non-negative integer."))
  else if n = 0 then l
  else match l with
       | [] -> []
       | hd::tl -> drop tl (n-1)

(* problem 7*)
let rec unzip_helper : ('a * 'b) list -> 'a list -> 'b list -> 'a list * 'b list
= fun l al bl ->
  match l with
  | [] -> (al, bl)
  | hd::tl -> let a = fst hd in
              let b = snd hd in
              unzip_helper tl (al @ [a]) (bl @ [b])

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> unzip_helper lst [] []

(* problem 8*)
let rec reverse : int list -> int list
= fun l -> fold (fun x a -> a @ [x]) l []

let rec change : int list -> int -> int
= fun coins amount ->
  if amount = 0 then 1
  else if amount < 0 then 0
  else match coins with
       | [] -> 0
       | _ -> let rev_coins = reverse coins in
              let largest = List.hd rev_coins in
              let remains = List.tl rev_coins in
              (change remains amount) + (change coins (amount - largest))

(*
let rec change2 : int list -> int -> int
= fun coins amount ->
  match coins with
  | [] -> 0
  | _ -> if amount = 0 then 1
         else if amount < 0 then 0
         else let rev_coins = reverse coins in
              let largest = List.hd rev_coins in
              let remains = List.tl rev_coins in
              (change2 remains amount) + (change2 coins (amount - largest))
*)
