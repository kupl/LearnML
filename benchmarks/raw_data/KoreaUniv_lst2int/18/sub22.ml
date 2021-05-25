let lst2int : int list -> int
= fun lst -> 
  (List.fold_left (fun x y  -> 10 * (x + y)) 0 lst) / 10 ;;