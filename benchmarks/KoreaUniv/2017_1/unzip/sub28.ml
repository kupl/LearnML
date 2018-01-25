(*problem 7. *)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
=fun lst ->

let rec pick1 l =
if l = [] then []
else
match l with
|[] -> []
|(hd1, hd2) ::tl -> hd1::(pick1 tl)

in
let rec pick2 l =
if l = [] then []
else
match l with
|[] -> []
|(hd1, hd2) ::tl -> hd2::(pick2 tl)

in
if lst = [] then ([],[])
else ((pick1 lst),(pick2 lst))