(* HW2 Exercise 4 Check Metro Map *)

type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string



let rec check : lambda -> bool =
  let rec checkList : lambda * var list -> bool =
    let rec isIdInList : var * var list -> bool = fun (id_checking, id_list) ->
      match id_list with
      | [] -> false
      | head :: tail ->
        if (head = id_checking) then true
        else isIdInList (id_checking, tail)
    in

    fun (lambda_checking, id_list) ->
      match lambda_checking with
      | V id -> (isIdInList (id, id_list))
      | P (id, lambda) -> (checkList (lambda, id :: id_list))
      | C (lambda1, lambda2) ->
        ((checkList (lambda1, id_list)) && (checkList (lambda2, id_list)))
  in

  fun lambda_checking -> checkList(lambda_checking, [])

