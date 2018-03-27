(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst ->
      match lst with
        | [] -> 0
        | hd::tl -> let getmax x y = if x > y then x else y in
          getmax hd (max tl);;


let rec min : int list -> int
= fun lst -> 
    match lst with
      | [] -> max_int
      | hd::tl -> let getmin x y = if x < y then x else y in
        getmin hd (min tl);;
