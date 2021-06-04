type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((aexp : aexp), (str : string)) : aexp =
  match aexp with
  | Const n -> Const 0
  | Var x -> if x = str then Const 1 else Const 0
  | Power (x, n) ->
      if x != str || n = 0 then Const 0
      else if x = str then Times [ Const n; Power (x, n - 1) ]
      else Const 0
  | Times alist -> (
      match alist with
      | [] -> raise Failure "Empty List"
      | [ hd ] -> diff (hd, str)
      | hd :: tl ->
          if diff (hd, str) = Const 0 then
            Times ([ hd ] @ [ diff (Times tl, str) ])
          else
            Sum
              [
                Times ([ diff (hd, str) ] @ tl);
                Times ([ hd ] @ [ diff (Times tl, str) ]);
              ] )
  | Sum alist -> (
      match alist with
      | [] -> raise Failure "Empty List"
      | [ hd ] -> diff (hd, str)
      | hd :: tl -> (
          if diff (hd, str) = Const 0 then diff (Sum tl, str)
          else
            match diff (Sum tl, str) with
            | Sum __s70 -> Sum ([ diff (hd, str) ] @ __s70)
            | Const 0 -> Sum [ diff (hd, str) ]
            | _ -> Sum ([ diff (hd, str) ] @ [ diff (Sum tl, str) ]) ) )
