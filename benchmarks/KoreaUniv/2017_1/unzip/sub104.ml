
(* problem 7*)

let first (x,y) = x
let second (x,y) = y

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> match lst with
  | [] -> ([],[])
  | hd::tl -> (first hd::first (unzip tl), second hd::second (unzip tl))
