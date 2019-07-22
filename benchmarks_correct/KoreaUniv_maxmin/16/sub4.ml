(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst ->
   match lst with
   | hd :: [] -> hd
   | hd :: tl -> big hd (max tl)
   and big a b =
      if a < b then b
      else a
 