(* problem 6*)

let drop : 'a list -> int -> 'a list
= fun l n -> (*TODO*)
  let rec aux i = function
      | [] -> []
      | h :: t -> if i = n then aux 1 t else h :: aux (i+1) t  in
    aux 1 l;;