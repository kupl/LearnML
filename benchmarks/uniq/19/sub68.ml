let uniq : 'a list -> 'a list
= fun lst -> (* TODO *)
let rec uniq x =
let rec uniq lst n = 
    match lst with
    | [] -> []
    | h :: t -> uniq t, n if (n = h) else (h :: (uniq(t, n)))
match x with
    |[] -> []
    | h::t -> uniq t, h
;;
