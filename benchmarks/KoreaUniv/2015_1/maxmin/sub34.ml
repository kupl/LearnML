let rec max : int list -> int
=fun l ->
match l with
[] -> 0
|hd::tl -> if hd > (max tl) then hd
else  (max tl);;

let rec min : int list -> int
=fun l ->
match l with
[] -> (max l)
|hd::tl -> if(hd < (min tl)) && (min tl != 0) then hd
else if (min tl) != 0 then (min tl)
else 777;;
