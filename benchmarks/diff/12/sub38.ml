type ae = 
  CONST of int
  | VAR of string
  | POWER of string * int
  | TIMES of ae list
  | SUM of ae list

let rec diff (exp, d) =
  match exp with
  CONST i -> CONST 0
  | VAR str -> 
      if str = d then (CONST 1)
      else (CONST 0)
  | POWER (str, i) ->
      if i == 0 then CONST 0
      else if (i == 1 & str = d) then CONST 1 
      else if str = d then (TIMES ((CONST i)::[POWER (str, (i - 1))]))
      else CONST 0
  | SUM sList ->
      (
        match sList with
        [] -> CONST 0
        | [hd] -> diff (hd, d)
        | (hd::tl) -> SUM [(diff (hd, d));(diff ((SUM tl), d))]
      )
  | TIMES tList ->
      (
        match tList with
        [] -> (CONST 0)
        | [hd] -> diff (hd, d)
        | (hd::tl) -> SUM [(TIMES ((diff (hd, d))::tl)) ; (TIMES [hd;(diff((TIMES tl), d))])]
      )
