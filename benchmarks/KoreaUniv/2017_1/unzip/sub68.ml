(*problem 7*)
let rec unzip_a :('a*'b) list -> 'a list
= fun lst ->
match lst with
| [] -> []
| (x,y)::tail -> x::unzip_a tail

let rec unzip_b :('a*'b) list -> 'b list
= fun lst ->
match lst with
| [] -> []
| (x,y)::tail -> y::unzip_b tail

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
match lst with
| [] -> ([],[])
|_-> (unzip_a lst , unzip_b lst)
