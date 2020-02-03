(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst ->  (* TODO *)
	match lst with
		[] -> failwith "Empty list"
		|head::tail -> 
			(* 하나만 남은 경우 출력 *)
			if (List.length lst = 1) then head 
			else
				(* 리스트의 제일 처음값이 두번째보다 크면 두번째를 뺀 리스트를 다시 재귀돌림 *)
				if(head >= List.nth tail 0) then max( head::(List.tl tail) )

				(* 리스트의 제일 처음값이 두번째보다 작으면 첫번째를 뺀 리스트를 다시 재귀돌림 *)
				else max tail

 