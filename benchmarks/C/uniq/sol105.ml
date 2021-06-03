let rec uniq a = 
match a with
| [] -> []
| h::t -> h::uniq (remove [h] a)
and remove a b = 
match b with
| [] -> []
| h::t -> if [h]=a then remove a t else h::remove a t;;

uniq [5;6;5;4];;
