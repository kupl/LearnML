let rec rmdup a b = 
match b with
| [] -> a
| h::t -> rmdup (remove [h] a) t
and remove a b = 
match b with
| [] -> []
| h::t -> if [h]=a then remove a t else h::remove a t;;

let app a b = b @ rmdup a b;;

app [4;5;6;7] [1;2;3;4];;
