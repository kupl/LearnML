type ae = CONST of int
  | VAR of string
  | POWER of string * int
  | TIMES of ae list
  | SUM of ae list ;;


exception InvalidArgument;;
let rec diff (expr, dstr) =
  match expr with
  | CONST i -> CONST(0)
  | VAR vstr -> if vstr = dstr then CONST(1) else CONST(0)
  | POWER (base, exponent) ->
    if (base = dstr) then
      TIMES([CONST(exponent);POWER(base, exponent-1)])
    else CONST(0) (* partial *)
  | TIMES ae_list ->
    let pick_over (f, lst) =
      let rec pick_over_inner (pred_list, ele, next_list) =
        if next_list = [] then [f(pred_list, ele, next_list)]
        else [f(pred_list, ele, next_list)] @ pick_over_inner(pred_list @ [ele], List.hd(next_list), List.tl(next_list)) in
      if lst = [] then [] else pick_over_inner([], List.hd(lst), List.tl(lst))
    in
    if ae_list = [] then raise InvalidArgument
    else SUM(List.map (fun x -> TIMES(x)) (pick_over ((fun (p, e, n) -> p @ [diff(e, dstr)] @ n), ae_list)))
  | SUM ae_list ->
    if ae_list = [] then raise InvalidArgument
    else SUM((List.map (fun x -> diff(x, dstr)) ae_list)) ;;
(*
let rec string_of_ae ae : ae -> string =
  match ae with
  | CONST i -> (string_of_int i)
  | VAR vstr -> vstr
  | POWER (base, exponent) -> Printf.sprintf "%s^%d" base exponent
  | TIMES ae_list -> List.fold_left (fun a b -> (String.concat a ["* ";(string_of_ae b)])) "" ae_list
  | SUM ae_list -> (List.fold_left (fun a b -> (String.concat "" [a;" + ";string_of_ae(b)])) "" ae_list);;

*)
