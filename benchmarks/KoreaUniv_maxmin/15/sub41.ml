let rec max l =
match l with
[] -> []
| [hd] -> hd
| hd::tl -> if hd > max tl then hd else max tl;;
let rec min l =
metch l with
[] -> []
| [hd] -> hd
| hd::tl -> if hd < min tl then hd else min tl;;

