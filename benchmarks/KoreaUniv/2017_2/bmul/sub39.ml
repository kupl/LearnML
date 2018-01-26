(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec bd a = 
    match a with
    | []->[]
    | hd::tl -> (bd tl)@[hd]

let rec dadd a1 a2 =
    match a1 with
    | []-> a2
    | h1::t1 -> 
        (match a2 with 
        | []-> a1
        | h2::t2 -> if h1=h2 then 
                        (if h1=ONE then ZERO::(dadd (dadd [ONE] t1) t2)
                        else (ZERO::dadd t1 t2))
                    else ONE::(dadd t1 t2))

let rec dmul a b =
    match b with
    | [] -> []
    | hd::tl -> if hd = ONE then dadd a (ZERO::(dmul a tl))
                else (ZERO::(dmul a tl))

let bmul : bin -> bin -> bin
= fun b1 b2 -> (* TODO *)
    bd (dmul (bd b1) (bd b2))
