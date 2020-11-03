let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
    match l1, l2 with
    |[], l2 -> l2
    |l1, [] -> l1
    |h1::t1, h2::t2 -> if h1 < h2 then h1::app t1 (h2::t2) else if h1 > h2 then h2::app (h1::t1) t2 else h1::app t1 t2;;