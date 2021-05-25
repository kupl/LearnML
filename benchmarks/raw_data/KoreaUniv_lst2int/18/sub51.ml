let rec reverse l = match l with | [] -> [] | h::t -> (reverse t)@[h];;

let rec summation = 
  fun lst ->
    match lst with
    | [] -> 0
    | hd::tl -> hd + (summation tl) * 10 

let rec lst2int : int list -> int
= fun lst -> summation (reverse lst);;

(*lst2int [2;3;4;5];;*)