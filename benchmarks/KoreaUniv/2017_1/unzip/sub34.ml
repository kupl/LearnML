let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
 
let get_x tl = match tl with
| (x,y) -> x
 
in
 
let get_y tl = match tl with
| (x,y) -> y
 
in
 
match lst with
| [] -> ([], [])
| (x,y)::tl -> ([x]@get_x(unzip(tl)), [y]@get_y(unzip(tl)))