
let rec merge : int list * int list -> int list = fun(l1, l2) ->
    if l1 == [] && l2 == [] then []
    else if l1 == [] && l2 != [] then l2
    else if l1 != [] && l2 == [] then l1
    else
        if List.hd(l1) < List.hd(l2) then List.hd(l2)::merge(l1, List.tl(l2))
        else List.hd(l1)::merge(List.tl(l1), l2)


(*
let a = [9;8;4;1]
let b = [10;3]
let c = [100;40;7;6;2]

open Printf
let rec print_list = fun l ->
    List.iter (printf "%d ") l;
    print_endline " "

let _ = print_list a
let _ = print_list b
let _ = print_list c
let _ = print_list(merge(a,b))
let _ = print_list(merge(a,c))
*)
