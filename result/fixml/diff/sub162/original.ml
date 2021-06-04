type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp =
 fun (exp, x) ->
  match exp with
  | Const n -> Const 0
  | Var str -> if str = x then Const 1 else Var str
  | Power (str, n) ->
      if str = x then Times [ Const n; Power (str, n - 1) ] else Const 0
  | Times lst ->
      let rec timedf : aexp list -> aexp list -> aexp list =
       fun lst ans ->
        match lst with
        | [] -> ans
        | hd :: tl ->
            let rec isvar : aexp list -> bool =
             fun lst ->
              match lst with
              | [] -> false
              | hd :: tl -> (
                  match hd with
                  | Times lst -> if isvar lst = true then true else isvar tl
                  | Sum lst -> if isvar lst = true then true else isvar tl
                  | Const n -> isvar tl
                  | Var str -> true
                  | Power (str, n) -> if str = x then true else false )
            in
            if isvar lst = true then
              match hd with
              | Const n -> timedf tl (ans @ [ Const n ])
              | _ -> timedf tl (ans @ [ diff (hd, x) ])
            else ans @ [ Const 0 ]
      in
      Times (timedf lst [])
  | Sum lst ->
      let rec sumdf : aexp list -> aexp list -> aexp list =
       fun lst ans ->
        match lst with [] -> ans | hd :: tl -> sumdf tl ans @ [ diff (hd, x) ]
      in
      Sum (sumdf lst [])


let _ = diff (Sum [ Power ("x", 2); Times [ Const 2; Var "x" ]; Const 1 ], "x")

let _ =
  diff
    ( Sum
        [
          Times [ Const 5; Power ("x", 3) ]; Times [ Const 2; Var "x" ]; Const 3;
        ],
      "x" )


let _ = diff (Times [ Const 5; Power ("x", 3) ], "x")
