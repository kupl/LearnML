type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

exception InvalidArgument

let rec diff_temp ((aexp : aexp), (str : string)) : aexp =
  match aexp with
  | Const n -> Const 0
  | Var str1 -> if str1 = str then Const 1 else Const 0
  | Power (str1, n) ->
      if str1 = str then Times [ Const n; Power (str1, n - 1) ] else Const 0
  | Times aexplist ->
      if List.tl aexplist = [] then diff_temp (List.hd aexplist, str)
      else
        Sum
          [
            Times
              [ diff_temp (List.hd aexplist, str); Times (List.tl aexplist) ];
            Times
              [ List.hd aexplist; diff_temp (Times (List.tl aexplist), str) ];
          ]
  | Sum aexplist ->
      if List.tl aexplist = [] then diff_temp (List.hd aexplist, str)
      else
        Sum
          [
            diff_temp (List.hd aexplist, str);
            diff_temp (Sum (List.tl aexplist), str);
          ]


let diff ((aexp : aexp), (str : string)) : aexp =
  match aexp with
  | Const n -> Const 0
  | Var str1 -> if str1 = str then Const 1 else Const 0
  | Power (str1, n) ->
      if str1 = str then Times [ Const n; Power (str1, n - 1) ] else Const 0
  | Times aexplist -> (
      match aexplist with
      | [] -> raise Failure "Exception(Template)"
      | [ __s63 ] -> diff_temp (aexp, str)
      | _ ->
          Sum
            [
              Times (diff_temp (List.hd aexplist, str) :: List.tl aexplist);
              Times
                [ List.hd aexplist; diff_temp (Times (List.tl aexplist), str) ];
            ] )
  | Sum aexplist ->
      if aexplist = [] then raise InvalidArgument
      else
        Sum (List.map (fun (__s65 : aexp) -> diff_temp (__s65, str)) aexplist)
