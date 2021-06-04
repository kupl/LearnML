type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec checkpow lst str =
  match lst with
  | [] -> 1
  | hd :: tl -> (
      match hd with
      | Power (a, b) -> if a = str then b else checkpow tl str
      | _ -> checkpow tl str )


let checktemp1 lst = match lst with [ Const t ] -> Const t | _ -> Times lst

let checktemp2 lst =
  match lst with [ hd ] -> hd | [] -> Const 0 | _ -> Sum lst


let rec checktime lst str func =
  match lst with
  | [] -> []
  | hd :: tl -> (
      match hd with
      | Const a -> Const (a * checkpow lst str) :: checktime tl str func
      | Var b ->
          if b = str then [] @ checktime tl str func
          else Var b :: checktime tl str func
      | Times c -> checktemp1 (checktime c str func) :: checktime tl str func
      | Sum f -> checktemp2 (func f str) :: checktime tl str func
      | Power (d, e) ->
          if d = str then
            if e = 2 then Var d :: checktime tl str func
            else if e <= 1 then checktime tl str func
            else Power (d, e - 1) :: checktime tl str func
          else if e = 1 then Var d :: checktime tl str func
          else Power (d, e) :: checktime tl str func )


let rec checksum lst str =
  match lst with
  | [] -> []
  | hd :: tl -> (
      match hd with
      | Const a -> [] @ checksum tl str
      | Var b ->
          if b = str then Const 1 :: checksum tl str
          else Var b :: checksum tl str
      | Times c -> checktemp1 (checktime c str checksum) :: checksum tl str
      | Sum f -> checktemp2 (checksum f str) :: checksum tl str
      | Power (d, e) ->
          if d = str then
            if e = 2 then Times [ Const e; Var d ] :: checksum tl str
            else if e <= 1 then checksum tl str
            else Times [ Const e; Power (d, e - 1) ] :: checksum tl str
          else if e = 1 then Var d :: checksum tl str
          else Power (d, e) :: checksum tl str )


let rec diff : aexp * string -> aexp =
 fun (exp, x) ->
  match exp with
  | Const a -> Const 0
  | Var b -> if b = x then Const 1 else Var b
  | Times c -> checktemp1 (checktime c x checksum)
  | Sum f -> checktemp2 (checksum f x)
  | Power (d, e) ->
      if d = x then
        if e = 2 then Times [ Const e; Var d ]
        else if e <= 1 then Const 1
        else Times [ Const e; Power (d, e - 1) ]
      else if e = 1 then Var d
      else Power (d, e)


let _ = diff (Sum [ Power ("x", 2); Times [ Const 2; Var "x" ]; Const 1 ], "x")

let _ =
  diff
    ( Sum
        [
          Times [ Const 2; Power ("x", 3) ];
          Times [ Const 6; Power ("x", 2); Power ("y", 2) ];
          Times [ Const 19; Var "x"; Var "h"; Power ("y", 3) ];
          Times [ Const 5; Var "y" ];
          Times [ Const 10; Power ("z", 1); Var "g"; Var "h"; Var "i" ];
          Const 1;
        ],
      "y" )


let _ = diff (Sum [ Times [ Const 2; Power ("y", 3) ]; Const 5 ], "y")

let _ = diff (Const 254, "x")

let _ = diff (Var "y", "x")

let _ = diff (Var "x", "x")

let _ = diff (Times [ Const 2; Var "x" ], "x")

let _ = diff (Sum [ Var "x"; Const 5 ], "x")
