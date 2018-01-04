(* School of Computer Science & Engineering
 * 2009-23151
 * 조성근
 * HW 1 - Exercise 4
 *)

let rec zipperN (a:int list list) = 
  let rec heads a = match a with 
      [] -> []
    | []::tl -> heads tl
    | (shd::stl)::tl -> shd::(heads tl)
  in
  let rec tails a = match a with
      [] -> []
    | []::tl -> tails tl
    | (shd::stl)::tl -> stl::(tails tl)
  in
    match a with
	[] -> []
      | _ -> (heads a)@(zipperN (tails a));;
