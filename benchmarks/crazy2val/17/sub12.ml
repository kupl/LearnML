(* Homework 2 - Exercise 2
 * 2011-10492 Jaeyeong Yang *)
type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let crazy2val: crazy2 -> int = fun c ->
  let pow: int * int -> int = fun (b, e) ->
    let rec powrec: int * int * int -> int = fun (res, base, exp) ->
      match exp with
      | _ when exp <= 0 -> res
      | 1 -> res * base
      | e -> powrec (res * base, base, exp - 1)
    in
    powrec(1, b, e)
  in
  let rec auc: int * crazy2 -> int = fun (ee, cc) ->
    match cc with
    | NIL -> 0
    | ZERO cl -> auc (ee + 1, cl)
    | ONE cl -> auc (ee + 1, cl) + pow (2, ee)
    | MONE cl -> auc (ee + 1, cl) - pow (2, ee)
  in
  auc (0, c)
