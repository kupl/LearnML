type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let c2i: crazy2 -> int = fun c ->
  match c with
  | ONE c1 -> 1
  | MONE c1 -> -1
  | _ -> 0

let nextdigit: crazy2 -> crazy2 = fun c ->
  match c with
  | ZERO c1 | ONE c1 | MONE c1 -> c1
  | NIL -> NIL

let rec onebitadd: crazy2 * crazy2 * crazy2 -> int = fun (a, b, c) ->
  (((c2i a) + (c2i b) + (c2i c)) mod 2)

let rec onebitcarry: crazy2 * crazy2 * crazy2 -> crazy2 = fun (a, b, c) ->
  match (((c2i a) + (c2i b) + (c2i c)) / 2) with
  | 1 -> ONE NIL
  | -1 -> MONE NIL
  | _ -> NIL

let rec fulladd: crazy2 * crazy2 * crazy2 -> crazy2 = fun (a, b, c) ->
  match (a, b) with
  | (NIL, NIL) -> c
  | _ -> (
    match (onebitadd (a, b, c)) with
    | 1 ->  ONE (fulladd (nextdigit a, nextdigit b, onebitcarry (a, b, c)))
    | -1 -> MONE (fulladd (nextdigit a, nextdigit b, onebitcarry (a, b, c)))
    | _ -> ZERO (fulladd (nextdigit a, nextdigit b, onebitcarry (a, b, c)))
  )

let rec crazy2add: crazy2 * crazy2 -> crazy2 = fun (a, b) ->
  fulladd (a, b, NIL)
