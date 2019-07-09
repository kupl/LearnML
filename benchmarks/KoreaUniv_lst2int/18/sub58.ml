let lst2int : int list -> int
= fun lst ->
  let rec l2i l = match l with
  | [] -> 1, 0
  | x :: xs ->
    let (m, r) = l2i xs in (10 * m, x * m + r)
  in let (m, r) = l2i lst in r
;;

lst2int [2;3;4;5];;
