(* merge: int list * int list -> int list *)
(* input, output both descending order *)
let rec merge(list1 ,list2) : int list = match list1 with
  | [] -> list2
  | hd1 :: tl1 -> (match list2 with
    | [] -> list1
    | hd2 :: tl2 ->
        if (hd1 >= hd2) then hd1 :: merge(tl1, list2)
        else hd2 :: merge(list1, tl2)
  )

(**

let rec printlist l = (match l with
 | [] -> ()
 | hd :: tl -> (print_int hd ; print_string " " ; printlist tl)
)

let one = [10;8;6;4;2;0] ;;
let two = [9;7;5;2] ;;

printlist (merge (one, two))

**)
