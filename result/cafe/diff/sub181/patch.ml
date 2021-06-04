type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (x : string)) : aexp =
  match exp with
  | Var a -> if a = x then Const 1 else Const 0
  | Power (a, b) ->
      if a = x then Times [ Const b; Power (a, b - 1) ] else Const 0
  | Sum lst ->
      let rec addition (l : aexp list) (result : aexp list) : aexp =
        match l with
        | [] -> Sum result
        | hd :: tl -> addition tl (result @ [ diff (hd, x) ])
      in
      addition lst []
  | Times lst -> (
      let copy_lst : aexp list = lst in

      let rec multiple (flag : int) (l : aexp list) (result : aexp list) : aexp
          =
        match l with
        | [] -> Sum result
        | hd :: tl ->
            if copy_lst = l then
              if flag = 0 then
                multiple 1
                  (tl @ [ hd ])
                  (result @ [ Times ([ diff (hd, x) ] @ tl) ])
              else Sum result
            else
              multiple 1
                (tl @ [ hd ])
                (result @ [ Times ([ diff (hd, x) ] @ tl) ])
      in

      match copy_lst with
      | [] -> Const 0
      | __s63 :: __s64 ->
          Sum
            [
              Times (diff (__s63, x) :: __s64);
              Times [ __s63; diff (Times __s64, x) ];
            ] )
  | Const a -> Const 0
