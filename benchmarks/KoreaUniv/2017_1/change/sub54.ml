(* problem 8*)
let change: int list -> int -> int
= fun coins amount -> let rec re_change: int list -> int -> int -> int -> int
                    = fun coins amount count value ->
                    match coins with
                    | [] -> 0
                    | [a] -> 1
                    | hd :: tl -> if count > value then 0
                                else 1 + (re_change coins (amount mod hd + hd*count) (count + 1) value) in
                    
                    let rec sort: int list -> int list
                    = fun l ->
                        match l with
                        | [] -> []
                        | hd :: tl -> let rec insert: int -> int list -> int list
                                        = fun a l -> 
                                        match l with
                                        | [] -> [a]
                                        | hd :: tl -> if hd > a then hd :: (insert a tl)
                                                    else a :: l in
                        insert hd (sort tl) in
                    match (sort coins) with
                    | [] -> 0
                    | [a] -> 1
                    | hd :: tl -> re_change tl amount 0 (amount / hd);;

