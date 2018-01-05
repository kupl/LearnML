(* C:\Users\saigoy\Desktop\merge.ml *)

let rec merge : int list * int list -> int list = fun (lhs, rhs) ->
  match ( lhs , rhs ) with
  | ([] , []) -> []
  | ([] , r) -> r
  | (l , []) -> l
  | (lhd::ltl , rhd::rtl) -> ( if( lhd > rhd ) then lhd::(merge ( ltl, rhs ) )
  				else rhd::(merge (lhs, rtl)) );;

