(* Dept. of Computer Science and Engineering, 2015-12055, An Dantae, 1-2 *)
let rec sigma f a b =
    if (a > b) then 0 else (f a) + sigma f (a+1) b
(* Test Code
let x = 1
let y = 10
let f i = i * i
let _ = print_endline (string_of_int (sigma (x,y,f)))
*)
