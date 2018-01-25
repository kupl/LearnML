(* problem 7*)
let unzip: ('a * 'b) list -> 'a list * 'b list
    = fun lst -> 
        let rec re_unzip: ('a * 'b) list -> ('a list * 'b list) -> 'a list * 'b list
        = fun lst (fls, sls) -> 
            match lst with
            | [] -> (fls, sls)
            | (x, y) :: tl -> re_unzip tl (fls @ [x], sls @ [y]) in
                re_unzip lst ([], []);;