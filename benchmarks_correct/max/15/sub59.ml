let rec max : int list -> int
=fun l -> match l with
        [] -> 0
       |h::[] -> h
       |h1::t1 -> match t1 with
                [] -> h1
               |h::[] -> if h > h1 then h else h1
               |h2::t2 -> if h1 > h2 then max (h1::t2) else max (h2::t2);;