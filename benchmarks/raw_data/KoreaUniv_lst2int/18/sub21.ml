let lst2int : int list -> int
= fun lst -> 
    let rec lst3int n lst =
        match lst with 
        | [] -> n
        | hd::tl -> lst3int ((n * 10 ) + hd) tl in
        lst3int 0 lst;;

lst2int [1;2;3];;