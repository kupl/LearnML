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