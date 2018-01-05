(* 2010-11753 snucse Taekmin Kim *)
(* HW 1-1 *)

let rec merge : int list * int list -> int list = fun (x, y) -> 
  match x, y with
  |hx::tx, hy::ty ->
      if(hx > hy) then
        hx::merge(tx, y)
       else if(hy > hx) then
        hy::merge(x, ty)
      else
        hx::hy::merge(tx, ty)
        (*hx::merge(tx, ty)*)
  |[], hy::ty -> hy::merge([], ty)
  |hx::tx, [] -> hx::merge(tx, [])
  |[], [] -> []

(*
let result = merge([5; 3; 1], [4; 2; 1] )

open Printf
let () = List.iter (printf "%d ") result

let a11 = merge ([7; 2; 1], [5; 4; 3]) 
let a12 = merge ([], []) 
let a13 = merge ([9; 2], []) 
let a14 = merge ([], [7; 3]) 
let a15 = merge ([5; 4; 3], [5; 4; 3]) 
let a16 = merge ([5; 3; 1], [8; 6; 4; 2; 0]) 

open Printf
let () = List.iter (printf "%d ") a11
let _ = print_endline("")
let () = List.iter (printf "%d ") a12
let _ = print_endline("")
let () = List.iter (printf "%d ") a13
let _ = print_endline("")
let () = List.iter (printf "%d ") a14
let _ = print_endline("")
let () = List.iter (printf "%d ") a15
let _ = print_endline("")
let () = List.iter (printf "%d ") a16
let _ = print_endline("")
*)
