let rec max : int list -> int
=fun l -> 
match l with
[a]-> a
|hd::tl-> 
if hd > (max tl) then hd
else max tl
|[]-> raise Not_found;;

let rec min : int list -> int
=fun l -> 
match l with
[a]-> a
|hd::tl-> 
if hd < (min tl) then hd
else min tl
|[]-> raise Not_found;;