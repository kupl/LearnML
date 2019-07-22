let rec max : int list -> int=fun l ->
match l with | [] -> 0 
| hd::tl -> if tl = [] then hd else let big = max tl in if hd>big then hd else big;;
 