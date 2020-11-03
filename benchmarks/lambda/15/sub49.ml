
type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let check : lambda -> bool =
  fun m ->
    let rec checkRec : lambda * var list -> bool =
      fun (m, l) ->
        match m with
        | V id ->
            List.exists (fun id_ -> id_ = id) l
        | P (id, m_sub) ->
            checkRec (m_sub, id::l)
        | C (m_left, m_right) ->
            checkRec (m_left, l) && checkRec (m_right, l)
    in checkRec (m, [])

