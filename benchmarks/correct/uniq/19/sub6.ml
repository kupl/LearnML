(*
Done
*)
let rec checkBinA : 'a list -> 'a -> bool 
= fun lst b ->
  match lst with
    | [] -> false
    | hd::tl ->
      if hd = b then true
      else
        checkBinA tl b;;

let rec find_duplicate : 'a list -> 'a list -> 'a list
= fun res lst ->
  match lst with
    | [] -> res
    | hd::tl -> if (checkBinA res hd) then
                  (
                    find_duplicate res tl
                  )
                else
                  (
                    (find_duplicate (res@[hd]) tl)
                  );;

let uniq : 'a list -> 'a list
= fun lst -> 
  find_duplicate [] lst;;