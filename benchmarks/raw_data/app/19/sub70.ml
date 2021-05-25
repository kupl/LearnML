let app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  let rl2 = List.rev l2 in
  let atomapp : 'a list -> 'a -> 'a list = 
    fun li x -> if List.exists (fun a -> a = x) l1 then li else x :: li
  in
  List.fold_left atomapp l1 rl2
;;