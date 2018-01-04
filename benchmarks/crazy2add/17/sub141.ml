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

let rec crazy2add: crazy2 * crazy2 -> crazy2 = fun (a, b) ->
  match a, b with
  | NIL, NIL -> NIL
  | _, NIL -> a
  | NIL, _ -> b
  | ONE ax, ONE bx -> ZERO(crazy2add (crazy2add (ONE NIL, ax), bx))
  | ONE ax, ZERO bx -> ONE(crazy2add (ax, bx))
  | ONE ax, MONE bx -> ZERO(crazy2add (ax, bx))
  | ZERO ax, ONE bx -> ONE(crazy2add (ax, bx))
  | ZERO ax, ZERO bx -> ZERO(crazy2add (ax, bx))
  | ZERO ax, MONE bx -> MONE(crazy2add (ax, bx))
  | MONE ax, ONE bx -> ZERO(crazy2add(ax, bx))
  | MONE ax, ZERO bx -> MONE(crazy2add(ax, bx))
  | MONE ax, MONE bx -> ZERO(crazy2add (crazy2add (MONE NIL, ax), bx))

(* TESTING FIELD BELOW *)

let _ = print_endline(string_of_int(crazy2val(crazy2add(ONE(MONE(ZERO(MONE NIL))),ONE(ZERO(ONE NIL))))))
let _ = print_endline(string_of_int(crazy2val(crazy2add(ONE(ONE(ZERO(ONE(ONE NIL)))),ONE(ONE(ZERO(ONE(ONE NIL))))))))
