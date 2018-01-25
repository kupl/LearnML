(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> (* TODO *)
  if x < 0 then raise (Failure "error")
  else if y < 0 then raise (Failure "error")
  else if x < y then raise (Failure "error")
  else
  (
    match y with
      | 0 -> 1
      | _ -> 
        if y = x then 1
        else pascal(x-1, y-1) + pascal(x-1, y)    
  )
  

  