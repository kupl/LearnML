(* let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> [] (* TODO *);;
*)

let rec compare
= fun a l2 ->
  match l2 with
      [] -> false
    | hd :: tl -> if (a = hd) then true 
                  else compare a (tl);;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  match l1, l2 with
      [], []-> []
    | l1 , []-> l1
    | [], l2 -> l2
    | l1_hd::l1_tl , l2_hd::l2_tl ->
      if (compare l1_hd l2 = false) then 
        let merged = l2 @ [l1_hd] in
          app l1_tl merged
      else app l1_tl l2;;
    