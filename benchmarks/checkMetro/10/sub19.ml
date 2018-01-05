
module type ORDERED_TYPE = 
	sig
		type t
		val compare: t -> t -> int
	end
;;
module type DIC_ELEMENT =
	sig
		type t
	end
;;
module Dictionary =
	functor( Input : ORDERED_TYPE ) -> functor( Item : DIC_ELEMENT ) ->
		struct
			type index = Input.t
			type item = Item.t
			type pair = index * item
			type dictree 	= Leaf of pair
							| Node of dictree * pair * dictree
							| NULL
			let empty = NULL
			let rec pushitem idx itm dic = 
				match dic with
					NULL -> Leaf (idx,itm)
					| Leaf (d,t) ->
						let cmp = compare d idx in
						if cmp = 0 then Leaf (idx,itm)
						else
							if cmp < 0 then Node (NULL,(d,t),(pushitem idx itm NULL))
							else Node ((pushitem idx itm NULL),(d,t),NULL)
					| Node (lt,(d,t),rt) ->
						let cmp = compare d idx in
						if cmp = 0 then Node (lt,(d,itm),rt)
						else
							if cmp < 0 then Node (pushitem idx itm lt,(d,t),rt)
							else Node (lt,(d,t),pushitem idx itm rt)
			let rec isIn idx dic =
				match dic with
					NULL -> false
					| Leaf (d,t) -> 
						if (compare d idx) = 0 then true
						else false
					| Node (lt,(d,t),rt) ->
						let cmp = compare d idx in
						if cmp = 0 then true
						else 
							if cmp < 0 then (isIn idx lt)
							else (isIn idx rt)
			let rec getItem idx dic =
				match dic with
					NULL -> raise Not_found
					| Leaf (d,t) -> 
						if (compare d idx) = 0 then t
						else raise Not_found
					| Node (lt,(d,t),rt) ->
						let cmp = compare d idx in
						if cmp = 0 then t
						else 
							if cmp < 0 then (getItem idx lt)
							else (getItem idx rt)
		end
;;

module OrderedString =
	struct
		type t = string
		let compare x y = compare x y
	end
;;
module IntType =
	struct
		type t = int
	end
;;
module MyDic = Dictionary(OrderedString)(IntType);;

type metro  = STATION of name
			| AREA of name * metro
			| CONNECT of metro * metro
	and name = string
;;

let rec checkup met areadic =
	match met with
		STATION n -> MyDic.isIn n areadic
		| AREA (n,m) -> checkup m (MyDic.pushitem n 0 areadic)
		| CONNECT (m1,m2) -> (checkup m1 areadic) && (checkup m2 areadic)

let checkMetro met =
	checkup met MyDic.empty
;;

