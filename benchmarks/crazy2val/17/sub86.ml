type crazy2=
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2
(*
let rec crazy2val (cz: crazy2) : int =
  let rec crazy2val_rec ((ret:int) , (czin:crazy2)) : int=
  match czin with
  | NIL -> ret
  | ZERO cz2 -> crazy2val_rec(ret*2, cz2)
  | ONE cz2 -> crazy2val_rec(ret*2+1, cz2)
  | MONE cz2 -> crazy2val_rec(ret*2-1, cz2)
  in 
  crazy2val_rec(0, cz)
*)

let rec crazy2val(cz:crazy2): int=
  match cz with
  | NIL -> 0
  | ZERO cz2 -> 2*(crazy2val cz2)
  | ONE cz2 -> 2*(crazy2val cz2)+1
  | MONE cz2 -> 2*(crazy2val cz2)-1
(* 
let _ = print_int (crazy2val(ZERO(ONE(MONE NIL))))  ; print_endline("")

let _ = print_int (crazy2val(ONE NIL))  ; print_endline("")

let _ = print_int (crazy2val(ONE(ZERO(ONE NIL))))  ; print_endline("")

let _ = print_int (crazy2val(ONE(MONE NIL)))  ; print_endline("")

let _ = print_int (crazy2val(ONE(MONE(ZERO(MONE NIL)))))  ; print_endline("")




*)
 
