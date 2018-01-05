type crazy2 = NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

let rec crazy2list: crazy2 -> int list = fun i ->
  match i with
  | NIL -> []
  | ZERO is -> 0 :: crazy2list is
  | ONE is -> 1 :: crazy2list is
  | MONE is -> -1 :: crazy2list is

let rec crazy2val': int list -> int = fun l ->
  match l with
  | [] -> 0
  | h :: t -> int_of_float(2.0**float_of_int(List.length(t)))*h + crazy2val' t

let crazy2val: crazy2 -> int = fun i -> crazy2val'(List.rev(crazy2list i))

(* TESTING FIELD BELOW *)

let _ = print_endline(string_of_int(crazy2val(ZERO(ONE(ONE(ZERO(ONE(ONE NIL))))))))
let _ = print_endline(string_of_int(crazy2val(ONE(ONE(ZERO(ONE(ONE NIL)))))))
