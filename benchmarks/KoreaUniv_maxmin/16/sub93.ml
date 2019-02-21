(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> match lst with
| [hd] -> hd
| hd :: hd' :: tl ->
if hd > hd' then max(hd :: tl)
else max(hd' :: tl);;


let rec min : int list -> int
= fun lst -> match lst with
| [hd] -> hd
| hd :: hd' ::tl ->
if hd < hd' then min(hd :: tl)
else min(hd' :: tl);;

