type aexp = Const of int
        | Var of string
        | Power of string * int
        | Times of aexp list
        | Sum of aexp list

exception InvalidArgument

let rec diff (xp, str) =
  match xp with
  | Const _ -> Const 0
  | Var str0 ->
      if str = str0 then Const 1
      else Const 0
  | Power (str0, n) ->
      if str = str0
      then Times [Const n; Power (str0, n-1)]
      else Const 0
  | Times [] -> raise InvalidArgument
  | Times [xp0] -> diff (xp0, str)
  | Times xp_ls ->
      Sum [Times ((diff (List.hd xp_ls, str))::(List.tl xp_ls));
           Times [List.hd xp_ls;
                  diff (Times (List.tl xp_ls), str)]]
  | Sum [] -> raise InvalidArgument
  | Sum [xp0] -> diff (xp0, str)
  | Sum xp_ls ->
      Sum (List.map (fun _xp -> diff (_xp, str)) xp_ls)
