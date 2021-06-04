type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let diff : aexp * string -> aexp =
 fun (e, x) ->
  let rec rev_list lst num =
    match lst with
    | hd :: tl ->
        if List.length tl = num then [] @ rev_list tl num
        else [ hd ] @ rev_list tl num
    | [] -> []
  in

  let rec tl_list e = match e with hd :: tl -> Sum tl | [] -> Sum [] in

  let rec differ e x =
    match e with
    | Const n -> Const 0
    | Var a -> if a = x then Const 1 else tl_list []
    | Power (a, b) ->
        if a = x then Times [ Const b; Power (a, b - 1) ] else Const 0
    | Times lst ->
        let rec times_diff lst2 =
          match lst2 with
          | hd :: tl ->
              if tl = [] then
                Times (differ hd x :: rev_list lst (List.length tl))
              else
                Sum
                  [
                    Times (differ hd x :: rev_list lst (List.length tl));
                    times_diff tl;
                  ]
          | [] -> Sum [ Const 0 ]
        in
        times_diff lst
    | Sum lst -> (
        match lst with
        | hd :: tl ->
            if tl = [] then Sum [ differ hd x ]
            else Sum [ differ hd x; differ (tl_list lst) x ]
        | [] -> Sum [ Const 0 ] )
  in
  differ e x
