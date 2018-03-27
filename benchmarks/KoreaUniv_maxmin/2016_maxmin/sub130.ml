(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a =
        match l with
        | [] -> a
        | hd::tl -> f hd (fold f tl a)

let rec max : int list -> int
= fun lst -> match lst with
        | [] -> min_int
        | hd::tl -> let tmp = max tl in
        if tmp > hd then tmp else hd;;

let rec min : int list -> int
= fun lst -> match lst with
        | [] -> max_int
        | hd::tl -> let tmp = max tl in
        if tmp < hd then tmp else hd;;
