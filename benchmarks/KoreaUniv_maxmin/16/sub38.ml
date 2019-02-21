(*********************)
(*     Problem 1     *)
(*********************)  
let rec max: int list -> int
= fun lst -> List.fold_right (fun x y -> if (x > y) then x else y) lst (List.hd lst)

let rec min : int list -> int
= fun lst -> List.fold_right (fun x y -> if (x > y) then y else x) lst (List.hd lst)
