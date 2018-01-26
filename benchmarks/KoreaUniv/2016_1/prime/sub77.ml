let rec prime : int -> bool
= fun n -> 
    if n <= 1 then false else
    let rec check_in_plist p plist =
        match plist with
        |   [] -> true
        |   h :: t -> if p mod h = 0 then false else check_in_plist p t in
    let rec loop i s plist = 
        if n > i
            then loop (i + s) 
                      (if s = 2 then 4 else 2)
                      (if check_in_plist i plist then i :: plist else plist)
            else check_in_plist n plist in
    match n with 
    |   1 | 4 -> false
    |   2 | 3 -> true
    |   _ -> loop 5 2 [3; 2]
