(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> match lst with
  | [] -> raise (Failure "The List is Empty")
  | hd::tl -> if tl = [] then hd
              else let maxcom x y = if x>y then x else y
                   in maxcom hd (max tl)

let rec min : int list -> int
= fun lst -> match lst with
  | [] -> raise (Failure "The List is Empty")
  | hd::tl -> if tl = [] then hd
              else let mincom x y = if x > y then y else x
                   in mincom hd (min tl)
