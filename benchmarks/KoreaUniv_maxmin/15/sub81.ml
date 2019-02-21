let first: int list -> int
= fun f ->
match f with
[] -> 0
|hd::tl -> hd;;

let switch: int * int list -> int list = fun (x, l) ->
match l with
[] -> x::[]
|hd::tl -> x::tl;;

let rec max: int list -> int = fun l ->
match l with
[] -> 0
|hd::tl -> if tl=[] then hd
else if hd > first tl then max(switch(hd, tl))
else max tl;;

let rec min: int list -> int = fun l ->
match l with
[] -> 0
|hd::tl -> if tl=[] then hd
else if hd < first tl then min(switch(hd,tl))
else min tl;;
