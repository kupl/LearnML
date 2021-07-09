type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec map_2 (f : 'a -> 'b -> 'a) a (lst : 'b list) =
  match lst with
  | [] -> a
  | hd :: tl ->
      let r = f a hd in
      map_2 f r tl


let get_type (exp : aexp) : int =
  match exp with
  | Const n -> n
  | Power (str, n) -> n
  | Sum lst -> List.length lst
  | _ -> 0


let get_list (exp : aexp) : aexp list =
  match exp with Sum lst -> lst | Times lst -> lst | _ -> []


let rec map_lst (f : aexp * string -> aexp) (exp : aexp) (str : string) :
    aexp list =
  let a : aexp list = get_list exp in

  match a with
  | [] -> []
  | hd :: tl ->
      let b : aexp = f (hd, str) in

      let c : aexp list = map_lst f (Sum tl) str in
      b :: c


let rec eval (time_times : aexp -> aexp -> aexp -> aexp)
    ((exp : aexp), (x : string)) : aexp =
  match exp with
  | Const n -> exp
  | Power (str, n) -> exp
  | Var str -> exp
  | Times lst -> (
      let a : aexp list = get_list exp in

      match a with
      | hd :: tl -> time_times (Times [ Const 1; Power ("x", 0) ]) hd (Times tl)
      | [] -> Times [] )
  | Sum lst ->
      let c : aexp list = map_lst (eval time_times) exp x in
      Sum c


let rec sum_time (time_sum : aexp -> aexp -> aexp list) (exp : aexp)
    (lst : aexp) : aexp list =
  let a : aexp list = get_list lst in

  match exp with
  | Const _ -> (
      match a with
      | hd :: tl -> Times [ exp; hd ] :: sum_time time_sum exp (Sum tl)
      | [] -> [] )
  | Var _ -> (
      match a with
      | hd :: tl -> Times [ exp; hd ] :: sum_time time_sum exp (Sum tl)
      | [] -> [] )
  | Power (_, _) -> (
      match a with
      | hd :: tl -> Times [ exp; hd ] :: sum_time time_sum exp (Sum tl)
      | [] -> [] )
  | Times _ -> (
      match a with
      | hd :: tl -> Times [ exp; hd ] :: sum_time time_sum exp (Sum tl)
      | [] -> [] )
  | Sum lst2 -> time_sum exp lst


let rec time_sum (lst1 : aexp) (lst2 : aexp) : aexp list =
  let b : aexp list = get_list lst2 in

  match b with
  | hd :: tl ->
      let a : aexp list = sum_time time_sum hd lst1 in
      List.rev_append a (time_sum lst1 (Sum tl))
  | [] -> []


let rec time_times (exp1 : aexp) (x : aexp) (exp2 : aexp) : aexp =
  let a : aexp list = get_list exp1 in

  let b : aexp list = get_list exp2 in

  match x with
  | Const n -> (
      let c : int = n in

      let d : int = get_type (List.nth a 0) in

      match b with
      | hd :: tl ->
          time_times (Times [ Const (d * c); List.nth a 1 ]) hd (Times tl)
      | [] -> Times [ Const (d * c); List.nth a 1 ] )
  | Var str -> (
      match List.nth a 1 with
      | Power (str, n) -> (
          match b with
          | hd :: tl ->
              time_times
                (Times [ List.nth a 0; Power (str, n + 1) ])
                hd (Times tl)
          | [] -> Times [ List.nth a 0; Power (str, n + 1) ] )
      | _ -> raise Failure "type error in time_times->Var()" )
  | Power (str, n) -> (
      let c : int = n in

      match List.nth a 1 with
      | Power (str, n) -> (
          match b with
          | hd :: tl ->
              time_times
                (Times [ List.nth a 0; Power (str, n + c) ])
                hd (Times tl)
          | [] -> Times [ List.nth a 0; Power (str, n + c) ] )
      | _ -> raise Failure "type error in time_times->Var()" )
  | Times lst -> (
      let p : aexp list = get_list x in

      match p with
      | [ Const int1; Power (str, int2) ] -> (
          let e : int = get_type (List.nth p 0) in

          let g : int = get_type (List.nth p 1) in

          let h : string = str in

          match b with
          | [] ->
              Times
                [
                  Const (get_type (List.nth a 0) * e);
                  Power (h, get_type (List.nth a 1) + g);
                ]
          | hd :: tl ->
              time_times
                (Times
                   [
                     Const (get_type (List.nth a 0) * e);
                     Power (h, get_type (List.nth a 1) + g);
                   ])
                hd (Times tl) )
      | hd :: tl ->
          let y : aexp =
            time_times (Times [ Const 1; Power ("x", 0) ]) hd (Times tl)
          in
          time_times exp1 y exp2
      | [] -> (
          match b with hd :: tl -> time_times exp1 hd (Times tl) | [] -> exp1 )
      )
  | Sum lst -> (
      match List.nth a 1 with
      | Power (str, n) -> (
          let h : string = str in

          let e : aexp list = sum_time time_sum exp1 x in

          let q : aexp = eval time_times (Sum e, h) in

          match b with
          | [] -> q
          | hd :: tl -> (
              let j : aexp =
                time_times (Times [ Const 1; Power ("x", 0) ]) hd (Times tl)
              in

              match j with
              | Times lst ->
                  let k : aexp list = sum_time time_sum j q in
                  eval time_times (Sum k, h)
              | Sum lst ->
                  let p : aexp list = time_sum j q in
                  eval time_times (Sum p, h)
              | _ -> raise Failure "Type error: j must be Times or Sum" ) ) )


let rec differ (exp : aexp) : aexp =
  match exp with
  | Const n -> Const 0
  | Var str -> Const 1
  | Power (str, n) ->
      if n != 0 then Times [ Const n; Power (str, n - 1) ]
      else Times [ Const n ]
  | Times lst -> (
      match lst with
      | [ Const int1; Power (str, int2) ] ->
          if int2 = 0 then Const 0
          else Times [ Const (int1 * int2); Power (str, int2 - 1) ]
      | _ -> raise Failure "type error in differ" )
  | Sum lst ->
      let a : aexp list = List.map differ lst in
      Sum a


let diff ((exp : aexp), (x : string)) : aexp =
  let a : aexp = eval time_times (exp, x) in
  differ a
