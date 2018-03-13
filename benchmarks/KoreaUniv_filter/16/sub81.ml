let rec filter : ('a -> bool) -> 'a list -> 'a list
= fun pred lst ->
let rec rfilter : 'a list -> 'a list
= fun l ->
match l with 
|[] -> []
|h::t -> if pred h = true then (h::rfilter t ) else rfilter t in
rfilter lst;;
