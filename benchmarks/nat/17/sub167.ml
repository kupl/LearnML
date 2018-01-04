type nat = ZERO | SUCC of nat

let rec natadd : nat * nat -> nat = fun (x, y) ->
  match x, y with
  | ZERO, _ -> y
  | _, ZERO -> x
  | SUCC xs, _ -> natadd (xs, SUCC y)

let rec natmul : nat * nat -> nat = fun (x, y) ->
  match x, y with
  | ZERO, _ -> ZERO
  | _, ZERO -> ZERO
  | SUCC ZERO, _ -> y
  | _, SUCC ZERO -> x
  | SUCC xs, _ -> natmul (xs, natadd (y, y))

(* TESTING FIELD BELOW *)

let ans =
  if natmul ((SUCC (SUCC ZERO)), (SUCC (SUCC ZERO))) = SUCC (SUCC (SUCC (SUCC ZERO)))
  then 1 else 0

let _ = print_endline(string_of_int(ans))
