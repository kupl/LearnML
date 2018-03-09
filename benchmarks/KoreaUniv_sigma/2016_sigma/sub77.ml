let rec sigma : (int -> int) -> int -> int -> int
= fun f a b ->
    if a > b then sigma f b a else
    let rec loop a r =
        if a = b 
            then r + f a
            else loop (a + 1) (r + f a) in
    loop a 0