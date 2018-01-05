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
        hx::merge(tx, ty)
  |[], hy::ty -> hy::merge([], ty)
  |hx::tx, [] -> hx::merge(tx, [])
  |[], [] -> []

(*
let result = merge([5; 3; 1], [4; 2; 1] )

open Printf
let () = List.iter (printf "%d ") result
*)
