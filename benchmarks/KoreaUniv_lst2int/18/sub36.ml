let rec lst2int : int list -> int
= fun lst -> match lst with
    | [] -> 0
    | (h::t) -> List.fold_right (fun num n -> num + n * 10) lst 0
    ;;

    lst2int[4;3;2;1];;