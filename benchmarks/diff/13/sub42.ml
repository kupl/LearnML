type ae = CONST of int
        | VAR of string
        | POWER of string * int
        | TIMES of ae list
        | SUM of ae list

exception InvalidArgument

let rec diff (ae, str) =
  let rec strip ae =
    match ae with
    | TIMES [] -> CONST 1
    | TIMES ((CONST 0)::tl) -> CONST 0
    | TIMES [hd;(CONST 0)] -> CONST 0
    | TIMES ((CONST 1)::tl) -> strip (TIMES tl)
    | TIMES [hd;(CONST 1)] -> hd
    | TIMES (hd::[]) -> hd
    | SUM [] -> CONST 0
    | SUM (hd::[]) -> hd
    | SUM [hd;(CONST 0)] -> hd
    | SUM ((CONST 0)::tl) -> strip (SUM tl)
    | _ -> ae in

  match ae with
  | CONST (i) -> CONST 0
  | VAR (s) ->
      if (s=str) then CONST 1
      else CONST 0
  | POWER (s, i) ->
      if s=str then strip (TIMES [CONST i; POWER (s, i-1)])
      else CONST 0
  | TIMES [] -> raise InvalidArgument
  | TIMES (hd::tl) -> strip (SUM [strip (TIMES ((diff (hd, str))::tl)); strip
  (TIMES [hd; diff (strip (TIMES  tl), str)])])
  | SUM [] -> raise InvalidArgument
  | SUM (a::b) -> (strip (SUM ([(diff (a, str));(diff ((strip (SUM b)), str))])))
