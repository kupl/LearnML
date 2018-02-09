(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> fold_for_Pro1 (fun x y -> if (x>y) then x else y) lst;;

let rec min : int list -> int
= fun lst -> fold_for Pro1 (fun x y -> if (x>y) then y else x) lst;;
