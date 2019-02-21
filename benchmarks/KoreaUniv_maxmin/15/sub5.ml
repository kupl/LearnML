(* Problem 3 *)
let rec length l = 
  match l with
  | [] -> 0
  | hd::tl -> 1 + length tl
  
let rec max : int list -> int
=fun l -> (* TODO *)  
  match l with
  | [] -> raise (Failure "error")
  | hd::tl -> 
    if (length l) = 1 then hd
    else
    (
      let a = max(tl) in
        if(hd > a) then hd
        else a      
    )
      
let rec min : int list -> int
=fun l -> (* TODO *)
  match l with
  | [] -> raise (Failure "error")
  | hd::tl -> 
    if (length l) = 1 then hd
    else
    (
      let a = min(tl) in
        if(hd > a) then a
        else hd
    )

