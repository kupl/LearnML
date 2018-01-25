(* problem 7*)

let fst (x,_) = x
let snd (_,x) = x

let rec make_first : ('a * 'b) list -> 'a list
= fun lst -> match lst with
            | [] -> []
            | hd::t1 -> (fst hd)::(make_first t1)
let rec make_second : ('a * 'b) list -> 'b list
= fun lst -> match lst with
            | [] -> []
            | hd::t1 -> (snd hd)::(make_second t1)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> let firstlst = make_first lst 
            in let secondlst = make_second lst
            in (firstlst, secondlst)