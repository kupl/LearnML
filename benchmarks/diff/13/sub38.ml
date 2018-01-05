type ae = CONST of int
        | VAR of string
        | POWER of string * int
        | TIMES of ae list
        | SUM of ae list

exception InvalidArgument

let rec diff (xp, str) =
  match xp with
  | CONST _ -> CONST 0
  | VAR str0 ->
      if str = str0 then CONST 1
      else CONST 0
  | POWER (str0, n) ->
      if str = str0
      then TIMES [CONST n; POWER (str0, n-1)]
      else CONST 0
  | TIMES [] -> raise InvalidArgument
  | TIMES [xp0] -> diff (xp0, str)
  | TIMES xp_ls ->
      SUM [TIMES ((diff (List.hd xp_ls, str))::(List.tl xp_ls));
           TIMES [List.hd xp_ls;
                  diff (TIMES (List.tl xp_ls), str)]]
  | SUM [] -> raise InvalidArgument
  | SUM [xp0] -> diff (xp0, str)
  | SUM xp_ls ->
      SUM (List.map (fun _xp -> diff (_xp, str)) xp_ls)
