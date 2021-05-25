(*
 * 2017 - 09 - 11
 * PL Homework 1-3
 * Joonmo Yang
*)

let iter(n, f) =
  let rec iter_sub(m, g, f') = 
    if m = 1 then g
    else if m < 1 then g
    else iter_sub(m-1, (fun x -> f' (g x)), f')
  in iter_sub(n, f, f)

(* tests
let fun2plus x = 2+x

let _ = print_int (iter(9,fun2plus) 2)
*)
