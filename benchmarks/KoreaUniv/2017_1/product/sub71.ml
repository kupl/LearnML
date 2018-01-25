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