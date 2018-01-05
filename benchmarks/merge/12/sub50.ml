
let rec merge l1 l2 =
    match l1, l2 with
    | [], _ -> l2
    | _, [] -> l1
    | h1::tl1, h2::tl2 ->
        if h1 > h2 then h1::(merge tl1 l2)
	else h2::(merge l1 tl2)
