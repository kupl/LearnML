(*********************)
(*     Problem 1     *)
(*********************)
let rec max lst=
match lst with 
[]->0
| [a]-> a
| hd::tl -> if hd > (max tl) then hd
            else (max tl);;  

let rec min lst=
match lst with 
[]->0
| [a]->a
| hd::tl -> if hd < (min tl) then hd
            else (min tl);;
