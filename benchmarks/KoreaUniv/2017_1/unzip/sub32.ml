(* problem 7*)

let rec unzip_rec lst rst =
        match lst with
        | [] -> rst
        | hd::tl -> let (head_a, head_b) = hd in
                let (rst_a, rst_b) = rst in
                unzip_rec tl (rst_a@[head_a], rst_b@[head_b])

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> unzip_rec lst ([],[])