let rec fold func l a =
        match l with
        | [] -> a
        | hd::tl -> func hd (fold func tl a)

let rec f : int list -> int
= fun lst -> match lst with
        | [] -> min_int
        | hd::tl -> let tmp = f tl in
        if tmp > hd then tmp else hd;;
