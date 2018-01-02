
let rec f  : int list -> int
= fun lst -> 
    match lst with
    |[] -> 0
    |hd::tl -> if hd > (f tl) then hd else (f tl) ;;
