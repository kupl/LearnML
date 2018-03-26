(* Problem 3 *)
let rec max : int list -> int
=fun l -> match l with
        | [] -> raise (Failure "List is Empty")
        | hd :: [] -> hd
        | hd :: tl -> if hd > max tl    then hd
                                        else max tl

let rec min : int list -> int
=fun l -> match l with
        | [] -> raise (Failure "List is Empty")
        | hd :: [] -> hd
        | hd :: tl -> if hd > min tl    then min tl
                                        else hd
