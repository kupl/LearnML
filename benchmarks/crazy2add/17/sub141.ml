type crazy2 = NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

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