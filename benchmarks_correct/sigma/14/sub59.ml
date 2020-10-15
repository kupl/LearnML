let rec sigma f a b  =
    if a>b then 0
    else f a + sigma f (a+1) b
;;
(*
let id_ftn x = x;;

print_endline (string_of_int (sigma ((1,100,id_ftn))))
*)
