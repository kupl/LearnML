let rec fold func l a =
        match l with
        | [] -> a
        | hd::tl -> func hd (fold func tl a)

let rec max : int list -> int
= fun lst -> match lst with
        | [] -> min_int
        | hd::tl -> let tmp = max tl in
        if tmp > hd then tmp else hd;;

let rec f : int list -> int
= fun lst -> match lst with
        | [] -> max_int
        | hd::tl -> let tmp = max tl in
        if tmp < hd then tmp else hd;;