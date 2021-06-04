type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec find_ch ((l : aexp list), (x : string)) : bool =
  match l with
  | [] -> false
  | hd :: tl -> (
      match hd with
      | Var str -> if str = x then true else find_ch (tl, x)
      | Power (str, _) -> if str = x then true else find_ch (tl, x)
      | _ -> find_ch (tl, x) )


let rec diff ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Sum l -> (
      match l with
      | [] -> Const 0
      | [ hd ] -> diff (hd, x)
      | hd :: tl -> Sum ([ diff (hd, x) ] @ [ diff (Sum tl, x) ]) )
  | Times l -> (
      match l with
      | [] -> raise Failure "Exception(Template)"
      | [ __s63 ] -> diff (__s63, x)
      | __s64 :: __s65 ->
          if diff (__s64, x) = Const 0 then
            Times ([ __s64 ] @ [ diff (Times __s65, x) ])
          else
            Sum
              [
                Times ([ diff (__s64, x) ] @ __s65);
                Times ([ __s64 ] @ [ diff (Times __s65, x) ]);
              ] )
  | Var v -> if v = x then Const 1 else Const 0
  | Power (v, n) ->
      if v = x then
        match n with
        | 1 -> Const 1
        | 2 -> Times ([ Const 2 ] @ [ Var v ])
        | _ -> Times ([ Const n ] @ [ Power (v, n - 1) ])
      else Const 0
  | Const n -> Const 0
