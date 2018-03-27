(*********************)
(*     Problem 1     *)
(*********************)

let rec max : int list -> int
= fun lst -> match lst with [] -> min_int | h::t -> let s = max t in if (s > h) then s else h

let rec min : int list -> int
= fun lst -> match lst with [] -> max_int | h::t -> let s = min t in if (s > h) then h else s
