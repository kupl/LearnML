let rec merge (l1, l2) =
    match l1 with
    | [] -> l2
    | hd::tl -> (
        match l2 with
        | [] -> l1
        | hd'::tl' -> (
            if hd > hd'
            then hd :: merge(tl, l2)
            else hd' :: merge(l1, tl')
            )
        )
