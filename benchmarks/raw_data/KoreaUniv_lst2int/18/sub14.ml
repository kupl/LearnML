exception EmptyList;;

let lst2int : int list -> int
= fun lst ->
  let temp = 0 in 
    let rec convert : int list -> int -> int
    = fun lst temp ->
      match lst with
      | [] -> raise EmptyList
      | hd::tl -> if (tl = []) then ((temp * 10) + hd) else (convert tl ((temp * 10) + hd)) in
    convert lst temp
;;


(*
I created the subfunction [convert : int list -> int -> int] to carry out the main part of the lst2int function.
It receives a holder variable 'temp' and an int list as arguments.

If the input list is an empty list, then we return the EmptyList exception message. Otherwise, we separate the list into hd and tl,
multiply the current temp variable by 10, and add hd to it. We will then call the function 'convert' recursively until we reach the base case, which is
lst = [].
*)