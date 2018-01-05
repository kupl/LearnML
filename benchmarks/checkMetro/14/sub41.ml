type metro = 
  | STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string

let checkMetro : metro -> bool = 
  let rec checkMetror : metro -> (name list) -> bool = 
    fun code vars ->
      match code with
      | STATION var -> (List.mem var vars)
      | CONNECT (code1, code2) ->
          (checkMetror code1 vars) && (checkMetror code2 vars)
      | AREA (new_var, code_in) -> checkMetror code_in (new_var::vars)
  in
  fun code -> checkMetror code []
