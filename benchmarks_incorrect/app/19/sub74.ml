let rec find e = function
    | [] -> false
    | h::t -> h = e || find e t
    
let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
    match l2 with
    | [] -> l1
    | h::t -> if find h l1 then app t l1
              else h :: app l1 t;;

app [4;5;6;7] [1;2;3;4];;

