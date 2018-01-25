(* problem 7*)
let rec help_unzip1 : ('a * 'b) list -> 'a list
= fun lst -> match lst with | [] -> []
                            | hd::tl -> match hd with (x,_) -> x::(help_unzip1 tl)

let rec help_unzip2 : ('a * 'b) list -> 'b list
= fun lst -> match lst with | [] -> []    
                            | hd::tl -> match hd with (_,x) -> x::(help_unzip2 tl)

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> match lst with | [] -> ([],[])
                            | hd::tl -> ((help_unzip1 lst) , (help_unzip2 lst))
