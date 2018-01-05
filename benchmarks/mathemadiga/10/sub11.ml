exception FreevarError;;
exception DividedByZero;;

type exp =
  | X
  | INT of int
  | REAL of float
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  | INTEGRAL of exp * exp * exp;;

let mathemadiga exp =
  let rec eval var exp =
    match exp with
    | X ->
      begin match var with
      | None -> raise FreevarError
      | Some x -> x
      end
    | INT x -> float_of_int x
    | REAL x -> x
    | ADD (x, y) -> eval var x +. eval var y
    | SUB (x, y) -> eval var x -. eval var y
    | MUL (x, y) -> eval var x *. eval var y
    | DIV (x, y) ->
      let x, y = eval var x, eval var y in
      if y <> 0.0 then x /. y else raise DividedByZero
    | SIGMA (INT start, INT stop, exp') when start <= stop ->
      let rec folder x i =
        if i > stop then x else
        folder (x +. eval (Some (float_of_int i)) exp') (i + 1)
      in
      folder 0.0 start
    | SIGMA _ -> assert false
    | INTEGRAL (start, stop, exp') ->
      let rec folder x i stop =
        if i >= stop then x else
        folder (x +. (eval (Some i) exp') *. (min 0.1 (stop -. i))) (i +. 0.1) stop
      in
      let start = eval var start and stop = eval var stop in
      if start > stop then ~-. (folder 0.0 stop start) else folder 0.0 start stop
  in
  eval None exp;;
