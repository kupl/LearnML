(*********************)
(*     Problem 1     *)
(*********************)
let rec max l =
match l with
| []-> 0
| hd::tl ->
if tl=[] then hd
else if hd< max tl then max tl
else hd;;


let rec min l =
match l with
| [] -> 0
| hd::tl ->
if tl=[] then hd
else if hd< min tl then hd
else min tl;;
