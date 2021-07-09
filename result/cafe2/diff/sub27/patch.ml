type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let diff ((f : aexp), (v : string)) : aexp =
  let rec deployEnv (env : (string * int * int) list) (o : int) : aexp list =
    match env with
    | a :: b -> (
        match a with
        | x, c, p ->
            if o = 0 && c = 0 then deployEnv b o
            else if x = "const" && o = 1 && c = 1 then deployEnv b o
            else if p = 0 then Const c :: deployEnv b o
            else if c = 1 && p = 1 then Var x :: deployEnv b o
            else if p = 1 then Times [ Const c; Var x ] :: deployEnv b o
            else if c = 1 then Power (x, p) :: deployEnv b o
            else Times [ Const c; Power (x, p) ] :: deployEnv b o )
    | [] -> []
  in

  let rec updateEnv (o : int) (e : string * int * int)
      (env : (string * int * int) list) : (string * int * int) list =
    match env with
    | hd :: tl -> (
        match hd with
        | s, c, p -> (
            match e with
            | s2, c2, p2 ->
                if o = 0 then
                  if s = s2 && p = p2 then (s, c + c2, p) :: tl
                  else hd :: updateEnv o e tl
                else if s = s2 then (s, c * c2, p + p2) :: tl
                else hd :: updateEnv o e tl ) )
    | [] -> [ e ]
  in

  let rec doDiff ((f : aexp), (v : string)) : aexp =
    match f with
    | Const _ -> Const 0
    | Var x -> if x = v then Const 1 else Const 0
    | Power (x, p) ->
        if p = 0 then Const 0
        else if x = v then Times [ Const p; Power (x, p - 1) ]
        else Const 0
    | Times x -> (
        match x with
        | [ a ] -> doDiff (a, v)
        | a :: b -> (
            let da : aexp = doDiff (a, v) in

            let db : aexp = doDiff (Times b, v) in

            match (a, db, b, da) with
            | Const p, Const q, [ Const r ], Const s -> Const ((p * q) + (r * s))
            | Const p, Const q, _, _ ->
                if da = Const 0 || b = [ Const 0 ] then Const (p * q)
                else Sum [ Const (p * q); Times (da :: b) ]
            | _, _, [ Const r ], Const s ->
                if a = Const 0 || db = Const 0 then Const (r * s)
                else Sum [ Times [ a; db ]; Const (r * s) ]
            | _ ->
                if a = Const 0 || db = Const 0 then Times (da :: b)
                else if b = [ Const 0 ] || da = Const 0 then Times [ a; db ]
                else Sum [ Times [ a; db ]; Times (da :: b) ] )
        | [] -> Const 0 )
    | Sum x -> Sum (List.map (fun (x : aexp) -> doDiff (x, v)) x)
  in

  let rec prettyAE (f : aexp) (env : (string * int * int) list) (o : int) :
      aexp list =
    match f with
    | Sum x -> (
        match x with
        | Const a :: z -> prettyAE (Sum z) (updateEnv 0 ("const", a, 0) env) 0
        | Var a :: z -> prettyAE (Sum z) (updateEnv 0 (a, 1, 1) env) 0
        | Power (a, b) :: z -> prettyAE (Sum z) (updateEnv 0 (a, 1, b) env) 0
        | Sum y :: z -> prettyAE (Sum (List.append y z)) env 0
        | Times y :: z -> (
            let l : aexp list = prettyAE (Times y) [] 1 in

            match l with
            | h :: t ->
                if t = [] then List.append l (prettyAE (Sum z) env 0)
                else List.append [ Times l ] (prettyAE (Sum z) env 0)
            | [] -> [] )
        | [] -> deployEnv env 0 )
    | Times x -> (
        match x with
        | Const a :: z -> prettyAE (Times z) (updateEnv 1 ("const", a, 0) env) 1
        | Var a :: z -> prettyAE (Times z) (updateEnv 1 (a, 1, 1) env) 1
        | Power (a, b) :: z -> prettyAE (Times z) (updateEnv 1 (a, 1, b) env) 1
        | Sum y :: z -> (
            let l : aexp list = prettyAE (Sum y) [] 0 in

            match l with
            | h :: t ->
                if t = [] then List.append l (prettyAE (Times z) env 1)
                else List.append [ Sum l ] (prettyAE (Times z) env 1)
            | [] -> [] )
        | Times y :: z -> prettyAE (Times (List.append y z)) env 1
        | [] -> deployEnv env 1 )
  in

  let res : aexp = doDiff (f, v) in
  doDiff (f, v)
