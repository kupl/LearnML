(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst ->
match lst with
|hd::tl -> if tl = [] then hd
              else if hd>max(tl) then hd
              else max(tl)
|[]-> raise (Failure "No element in list")


let rec min : int list -> int
= fun lst ->
match lst with
|hd::tl -> if tl = [] then hd
              else if hd<min(tl) then hd
              else min(tl)
|[]-> raise (Failure "No element in list")
