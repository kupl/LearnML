let rec reverse l =
match l with
| [] -> []
| hd::tl -> (reverse tl) @ [hd];;
let lst2int : int list -> int
= fun lst ->

  let rec sum l =
match l with
| [] -> 0
| hd::tl -> hd + 10*(sum tl) in

sum(reverse lst);;

    lst2int [2;3;4;5] ;;