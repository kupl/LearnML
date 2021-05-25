(* Dept. of Computer Science and Engineering, 2015-12055, An Dantae, 1-2 *)
let rec sigma : int * int * (int -> int) -> int = fun (a,b,f) ->
    if (a > b) then 0 else (f a) + sigma (a+1,b,f)
(* Test Code
let x = 1
let y = 10
let f i = i * i
let _ = print_endline (string_of_int (sigma (x,y,f)))
*)
