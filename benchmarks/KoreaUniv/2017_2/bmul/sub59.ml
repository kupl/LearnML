(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec bi2int : bin -> int = fun bi ->
    if List.length bi = 1 then
        match bi with
        |hd::tl -> if hd = ZERO then 0 else 1
    else
        match bi with
        |hd::tl ->
            match hd with
            |ZERO -> 2 * bi2int(tl)
            |ONE -> 2 * bi2int(tl) + 1

let rec int2bi : int -> bin -> bin = fun i bi ->
    if i = 0 then bi
    else if i mod 2 = 0 then int2bi (i/2) ([ZERO]@bi) else int2bi (i/2) ([ONE]@bi)

let bmul : bin -> bin -> bin
= fun b1 b2 ->
    let b1 = List.rev b1
    in let b2 = List.rev b2
    in int2bi ((bi2int b1) * (bi2int b2)) [];;

