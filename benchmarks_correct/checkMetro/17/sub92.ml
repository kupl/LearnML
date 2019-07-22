(* HW2 Exercise 4 Check Metro Map *)

type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string



let rec checkMetro : metro -> bool =
  let rec checkList : metro * name list -> bool =
    let rec isIdInList : name * name list -> bool = fun (id_checking, id_list) ->
      match id_list with
      | [] -> false
      | head :: tail ->
        if (head = id_checking) then true
        else isIdInList (id_checking, tail)
    in

    fun (metro_checking, id_list) ->
      match metro_checking with
      | STATION id -> (isIdInList (id, id_list))
      | AREA (id, metro) -> (checkList (metro, id :: id_list))
      | CONNECT (metro1, metro2) ->
        ((checkList (metro1, id_list)) && (checkList (metro2, id_list)))
  in

  fun metro_checking -> checkList(metro_checking, [])

