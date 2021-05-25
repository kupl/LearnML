(*
 * 2017 - 09 - 22
 * PL Homework 2-3
 * Joonmo Yang
*)

type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

(* make reverse crazy2. *)
let rec rev_crz : crazy2 * crazy2 -> crazy2 = 
  fun (crz, rst) ->
  ( match crz with
    | NIL -> rst
    | ZERO c -> rev_crz(c, ZERO rst)
    | ONE c -> rev_crz(c, ONE rst)
    | MONE c -> rev_crz(c, MONE rst)
  )

let crazy2add : crazy2 * crazy2 -> crazy2 =
  fun (crz1, crz2) ->
  (
    let rec crazy2add_sub : crazy2 * crazy2 * crazy2 * crazy2 -> crazy2 =
      fun (c1, c2, crry, csum) ->
        ( match c1, c2 with
          | NIL, NIL -> 
            ( match crry with
              | NIL -> csum 
              | ZERO _ -> ZERO csum 
              | ONE _ -> ONE csum 
              | MONE _ -> MONE csum
            )
          | NIL, ZERO c -> 
            ( match crry with
              | NIL -> crazy2add_sub(NIL, c, NIL, ZERO csum)
              | ZERO _ -> crazy2add_sub(NIL, c, NIL, ZERO csum)
              | ONE _ -> crazy2add_sub(NIL, c, NIL, ONE csum) 
              | MONE _ -> crazy2add_sub(NIL, c, NIL, MONE csum)
            )
          | NIL, ONE c ->
            ( match crry with
              | NIL -> crazy2add_sub(NIL, c, NIL, ONE csum)
              | ZERO _ -> crazy2add_sub(NIL, c, NIL, ONE csum)
              | ONE _ -> crazy2add_sub(NIL, c, ONE NIL, ZERO csum) 
              | MONE _ -> crazy2add_sub(NIL, c, NIL, ZERO csum)
            )
          | NIL, MONE c ->
            ( match crry with
              | NIL -> crazy2add_sub(NIL, c, NIL, MONE csum)
              | ZERO _ -> crazy2add_sub(NIL, c, NIL, MONE csum)
              | ONE _ -> crazy2add_sub(NIL, c, NIL, ZERO csum) 
              | MONE _ -> crazy2add_sub(NIL, c, MONE NIL, ZERO csum)
            )
          | ZERO c1z, NIL -> 
            ( match crry with
              | NIL -> crazy2add_sub(c1z, NIL, NIL, ZERO csum)
              | ZERO _ -> crazy2add_sub(c1z, NIL, NIL, ZERO csum)
              | ONE _ -> crazy2add_sub(c1z, NIL, NIL, ONE csum) 
              | MONE _ -> crazy2add_sub(c1z, NIL, NIL, MONE csum)
            )
          | ZERO c1z, ZERO c2z ->
            ( match crry with
              | NIL -> crazy2add_sub(c1z, c2z, NIL, ZERO csum)
              | ZERO _ -> crazy2add_sub(c1z, c2z, NIL, ZERO csum)
              | ONE _ -> crazy2add_sub(c1z, c2z, NIL, ONE csum) 
              | MONE _ -> crazy2add_sub(c1z, c2z, NIL, MONE csum)
            )
          | ZERO c1z, ONE c2o ->
            ( match crry with
              | NIL -> crazy2add_sub(c1z, c2o, NIL, ONE csum)
              | ZERO _ -> crazy2add_sub(c1z, c2o, NIL, ONE csum)
              | ONE _ -> crazy2add_sub(c1z, c2o, ONE NIL, ZERO csum) 
              | MONE _ -> crazy2add_sub(c1z, c2o, NIL, ZERO csum)
            )
          | ZERO c1z, MONE c2m ->
            ( match crry with
              | NIL -> crazy2add_sub(c1z, c2m, NIL, MONE csum)
              | ZERO _ -> crazy2add_sub(c1z, c2m, NIL, MONE csum)
              | ONE _ -> crazy2add_sub(c1z, c2m, NIL, ZERO csum) 
              | MONE _ -> crazy2add_sub(c1z, c2m, MONE NIL, ZERO csum)
            )
          | ONE c1o, NIL ->
            ( match crry with
              | NIL -> crazy2add_sub(c1o, NIL, NIL, ONE csum)
              | ZERO _ -> crazy2add_sub(c1o, NIL, NIL, ONE csum)
              | ONE _ -> crazy2add_sub(c1o, NIL, ONE NIL, ZERO csum) 
              | MONE _ -> crazy2add_sub(c1o, NIL, NIL, ZERO csum)
            )
          | ONE c1o, ZERO c2z -> 
            ( match crry with
              | NIL -> crazy2add_sub(c1o, c2z, NIL, ONE csum)
              | ZERO _ -> crazy2add_sub(c1o, c2z, NIL, ONE csum)
              | ONE _ -> crazy2add_sub(c1o, c2z, ONE NIL, ZERO csum) 
              | MONE _ -> crazy2add_sub(c1o, c2z, NIL, ZERO csum)
            )
          | ONE c1o, ONE c2o ->
            ( match crry with
              | NIL -> crazy2add_sub(c1o, c2o, ONE NIL, ZERO csum)
              | ZERO _ -> crazy2add_sub(c1o, c2o, ONE NIL, ZERO csum)
              | ONE _ -> crazy2add_sub(c1o, c2o, ONE NIL, ONE csum) 
              | MONE _ -> crazy2add_sub(c1o, c2o, NIL, ZERO csum)
            )
          | ONE c1o, MONE c2m ->
            ( match crry with
              | NIL -> crazy2add_sub(c1o, c2m, NIL, ZERO csum)
              | ZERO _ -> crazy2add_sub(c1o, c2m, NIL, ZERO csum)
              | ONE _ -> crazy2add_sub(c1o, c2m, NIL, ONE csum) 
              | MONE _ -> crazy2add_sub(c1o, c2m, NIL, MONE csum)
            )
          | MONE c1m, NIL ->
            ( match crry with
              | NIL -> crazy2add_sub(c1m, NIL, NIL, MONE csum)
              | ZERO _ -> crazy2add_sub(c1m, NIL, NIL, MONE csum)
              | ONE _ -> crazy2add_sub(c1m, NIL, NIL, ZERO csum) 
              | MONE _ -> crazy2add_sub(c1m, NIL, MONE NIL, ZERO csum)
            )
          | MONE c1m, ZERO c2z ->
            ( match crry with
              | NIL -> crazy2add_sub(c1m, c2z, NIL, MONE csum)
              | ZERO _ -> crazy2add_sub(c1m, c2z, NIL, MONE csum)
              | ONE _ -> crazy2add_sub(c1m, c2z, NIL, ZERO csum) 
              | MONE _ -> crazy2add_sub(c1m, c2z, MONE NIL, ZERO csum)
            )
          | MONE c1m, ONE c2o ->
            ( match crry with
              | NIL -> crazy2add_sub(c1m, c2o, NIL, ZERO csum)
              | ZERO _ -> crazy2add_sub(c1m, c2o, NIL, ZERO csum)
              | ONE _ -> crazy2add_sub(c1m, c2o, NIL, ONE csum) 
              | MONE _ -> crazy2add_sub(c1m, c2o, NIL, MONE csum)
            )
          | MONE c1m, MONE c2m ->
            ( match crry with
              | NIL -> crazy2add_sub(c1m, c2m, MONE NIL, ZERO csum)
              | ZERO _ -> crazy2add_sub(c1m, c2m, MONE NIL, ZERO csum)
              | ONE _ -> crazy2add_sub(c1m, c2m, NIL, MONE csum) 
              | MONE _ -> crazy2add_sub(c1m, c2m, MONE NIL, MONE csum)
            ) 
        )
    in rev_crz(crazy2add_sub(crz1, crz2, NIL, NIL), NIL)
  )
