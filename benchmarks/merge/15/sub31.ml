(* 2011-10915 / 생명과학부 / 신지민 / Homework 1-1 *)

let rec merge : int list * int list -> int list = fun (l1, l2) ->
	match (l1,l2) with
	|(_,[]) -> l1
	|([],_) -> l2
	|(hd1::tl1,hd2::tl2) -> begin
		if(hd1>hd2) then hd1::merge(tl1,l2)
		else hd2::merge(l1,tl2)		
			end

(*
open Printf
let a = merge ([7;5;3;1],[6;4;2])
let () = List.iter (printf "%d ") a

let _= print_endline("\n")

let a = merge ([],[6;4;2])
let () = List.iter (printf "%d ") a
let _= print_endline("\n")
let a = merge ([7;5;3],[])
let () = List.iter (printf "%d ") a
let _= print_endline("\n")
let a = merge ([4;3;2;1],[10;9;8;7;6])
let () = List.iter (printf "%d ") a
let _= print_endline("\n")
let a = merge ([100;94;50;45;20;1],[101101010;567;23;4;3])
let () = List.iter (printf "%d ") a
*)
