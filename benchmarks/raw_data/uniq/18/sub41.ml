let rec isin x l =
    match l with
    | [] -> false
    | h::t -> h = x || isin x t;;

let rec uniq_inner seen l =
    match l with
    | [] -> []
    | h::t -> if isin h seen
                then uniq_inner seen t
                else h :: uniq_inner (h::seen) t;;

let rec uniq : 'a list -> 'a list
= fun lst -> uniq_inner [] lst;;

uniq [5;6;5;4];;
uniq [1;1;2;2;3;3;4;4;5;5];;