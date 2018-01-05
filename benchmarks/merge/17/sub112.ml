(* ex 1 *)
let merge : int list * int list -> int list = fun (a, b) ->
  let rec merge_rec_rev : int list * int list * int list -> int list = fun (a, b, r) ->
    match (a, b) with
    |([], []) -> r
    |(ah :: at, []) -> merge_rec_rev(at, b, ah :: r)
    |([], bh :: bt) -> merge_rec_rev(a, bt, bh :: r)
    |(ah :: at, bh :: bt) -> 
      if ah > bh
        then merge_rec_rev(at, b, ah :: r)
        else merge_rec_rev(a, bt, bh :: r)
  in
  List.rev(merge_rec_rev(a, b, []))

(* ex 1 test *)
(*
let a : int list = 5 :: 2 :: 1 :: []
let b : int list = 4 :: 3 :: []
let merged : int list = merge(a,b)
let _ = List.iter (print_endline) (List.map string_of_int merged)
*)
