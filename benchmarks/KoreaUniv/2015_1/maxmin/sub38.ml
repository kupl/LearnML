let rec max l =
match l with
[x] -> x
lhd::tl->
if(hd > max(tl)) then hd
else max tl
|[] -> 0;;


let rec min l =
match l with
[x] -> x
lhd::tl->
if(hd < min(tl)) then hd
else min tl
|[] -> 0;;

