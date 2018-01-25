
(* problem 7*)
let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
  match lst with
  |[] -> ([], [])
  |hd::tl -> match hd with
            |(x,y) -> match unzip tl with
                      |(a,b) -> (x::a,y::b);;
