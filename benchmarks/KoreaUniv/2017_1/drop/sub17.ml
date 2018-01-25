
(* problem 6*)

let drop : 'a list -> int -> 'a list
= fun l n -> (*TODO*)
  let rec impl _l _n =
    match _l with
    | [] -> []
    | hd::tl ->
    if _n = 0 then _l
    else impl tl (_n - 1) in
  impl l n;;
