let rec merge : int list * int list -> int list = fun (l1, l2) -> (*merge: int list * int list -> int list*)
  match l1 with 
  | [] -> l2
  | hd1::rest1 -> match l2 with
                | [] -> l1
                | hd2::rest2 -> if hd1>hd2 then hd1::hd2::(merge (rest1, rest2)) else hd2::hd1::(merge (rest1, rest2))
;;
