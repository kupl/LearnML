(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> match lst with
  | [] -> 0
  | x::[] -> x
  | x::tl -> let y = max tl in
                if y > x then y else x;;
(* TODO *)

let rec min : int list -> int
= fun lst -> match lst with
  | [] -> 0
  | x::[] -> x
  | x::tl -> let y = min tl in
                if y < x then y else x;;
(* TODO *)
