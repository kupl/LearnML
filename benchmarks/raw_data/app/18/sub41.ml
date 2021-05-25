let rec isin x l =
    match l with
    | [] -> false
    | h::t -> h = x || isin x t;;

let rec subtractfst l1 l2 =
    match l1 with
    | [] -> []
    | h::t -> if isin h l2
                then subtractfst t l2
                else h :: subtractfst t l2;;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> let m1 = subtractfst l1 l2 in
        l2 @ m1;;
        
app [4;5;6;7] [1;2;3;4];;
app [4;5;6;7] [1;2;5;7];;