(*Problem 7*)
let fst (x,_) = x
let snd (_,x) = x

let rec tuple1 lst=
match lst with
|[] -> []
|hd::tl -> if(tl=[]) then fst hd::[] else (fst hd)::(tuple1 tl)

let rec tuple2 lst=
match lst with
|[] -> []
|hd::tl -> if(tl=[]) then snd hd::[] else (snd hd)::(tuple2 tl)


let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> (tuple1 lst, tuple2 lst)