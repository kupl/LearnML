let rec reverse : int list -> int list
= fun lst ->
  match lst with
    | [] -> []
    | hd :: tl -> (reverse tl) @ [hd] ;;

let rec powten : int -> int
= fun a -> if a <= 10 then 10 else 10 * powten(a / 10) ;;

let rec lst2int : int list -> int
= fun lst ->
  let rec calc : int list -> int
  = fun lst ->
    match lst with
      | [] -> 0
      | hd :: tl -> hd + powten(hd) * calc tl in
  calc (reverse lst) ;;