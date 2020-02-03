(*2014130211 Donghyun Koh*)



(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a =
  match l with
    | [] -> a
    | hd::tl -> f hd (fold f tl a)

let rec max : int list -> int
  = fun lst -> 
    let large a b =
      if a > b then a
      else b in fold (large)lst 0