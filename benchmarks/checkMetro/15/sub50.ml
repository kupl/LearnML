
type metro =
  | STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string

let checkMetro : metro -> bool =
  fun m ->
    let rec checkMetroRec : metro * name list -> bool =
      fun (m, l) ->
        match m with
        | STATION id ->
            List.exists (fun id_ -> id_ = id) l
        | AREA (id, m_sub) ->
            checkMetroRec (m_sub, id::l)
        | CONNECT (m_left, m_right) ->
            checkMetroRec (m_left, l) && checkMetroRec (m_right, l)
    in checkMetroRec (m, [])

