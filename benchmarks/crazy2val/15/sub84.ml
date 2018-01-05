(*
  CSE/2015-21233/김종권
  Homework 2-1
*)
type crazy2 =
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

let power n x =
  let rec power' n x acc =
    if x = 0 then acc
    else power' n (x-1) (n*acc)
  in
  power' n x 1

let rec crazy2val' c n acc =
  match c with
  | NIL -> acc
  | ZERO c' -> crazy2val' c' (n+1) ((0 * (power 2 n)) + acc)
  | ONE c' -> crazy2val' c' (n+1) ((1 * (power 2 n)) + acc)
  | MONE c' -> crazy2val' c' (n+1) ((-1 * (power 2 n)) + acc)


let crazy2val c =
  crazy2val' c 0 0
