(* problem 2*)
let rec smallest_divisor: int -> int
= fun n -> 
    let rec mini_divisor a b = 
        if a >= b * b then 
            match a mod b with
            |0 -> b
            |_ -> mini_divisor a (b+1)
        else a
    in mini_divisor n 2;;