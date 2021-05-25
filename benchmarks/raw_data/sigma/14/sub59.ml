let rec sigma (a, b, f) =
    if a>b then 0
    else f a + sigma(a+1, b, f)
;;
(*
let id_ftn x = x;;

print_endline (string_of_int (sigma ((1,100,id_ftn))))
*)
