type lambda = Var of string | Abs of string * lambda | App of lambda * lambda;;


let lambda_of_string s = 

	let s = s ^ ";" in
	let pos  = ref 0 in
	let get() = s.[!pos] in
	let next() = if !pos < String.length s - 1 then pos := !pos + 1 else failwith "oops" in
	let eat x  = if get() <> x then failwith "stop eating" else next() in

	let parse_ident_str()  = 
		let rec  rec_parse s = 
			if ((get() >='0') && (get() <= '9')) then
				let cur_dig = get() in
			 	next();
				rec_parse  (s^ (String.make 1 (cur_dig))) 
				else s in
		let cur =  String.make 1 (get ()) in
		next();
		rec_parse cur in 
	
	let parse_ident() = 
	        Var(parse_ident_str()) in

	let rec parse_abs() = 
		eat '\\';
		let v = parse_ident_str () in
		eat '.';
		let l = parse_lambda() in
		Abs(v, l) 

	and  parse res  =  if ((!pos =  String.length s - 1) || ')' = get())
				then res else  App(res, parse_lambda()) 
	and parse_lambda() = 
		match (get()) with 
		'\\' -> (let res = parse_abs() in
				parse res)
		|'(' -> (eat '('; 
			let res = parse_lambda() in
			eat ')';
			parse res)
		|_   ->  (let res = parse_ident() in
				parse res) in
	parse_lambda();;

let string_of_lambda lambda = 
	let rec to_string lambda s = 
		match lambda with 
			| Var (x) -> s ^"("^x^")"
			| Abs (x, y) -> s^"(\\"^x^"."^(to_string y "")^")"
			| App (x, y) -> s^"("^(to_string x "")^(to_string y "")^")" in
	to_string lambda "";;

let rec subst lambda oldVar newVar = 
	print_string (string_of_lambda(lambda)^ " "^oldVar^" "^newVar^" \n");
	match lambda with 
	|Var(x) -> if (x = oldVar) then Var(newVar) else Var(x)
	|App(x, y) -> App(subst x oldVar newVar, subst y oldVar newVar) 
	|Abs(x, y) -> 
		if (x = oldVar) then Abs(x, y) else Abs(x, subst y oldVar newVar) 

let is_alpha_equivalent x y = 

	let counter = ref 0 in
	
	let name_generation () =
        	let ret = "t"^ string_of_int !counter in
       		counter := !counter + 1;
  		ret in

	let rec make x y = match (x, y) with
		|(Var xx, Var yy) -> (xx = yy)
		|(App(x1, x2), App(y1, y2)) ->
			((make x1 y1) && (make  x2  y2))
		|(Abs(x1, x2), Abs(y1, y2)) -> 
			let next_free_character = (name_generation()) in
			make (subst x2 x1 next_free_character) (subst y2 y1 next_free_character)
		|_ -> false in

	make x y;;

let rec free lambda str  = 
	match lambda with
	|Var(x) -> true 
	|App(x, y) -> (free x str) && (free y str)
	|Abs(x, y) ->
		if (x = str) then false  else free y str;;


module My_set = Set.Make (String);;

let free_subst l1 l2 str = 
	
	let rec all_free_vars l blocked =
		match l with
			Var v ->
				if My_set.mem v blocked 
					then My_set.empty
					else My_set.singleton v
			| Abs(v, x) ->
				all_free_vars x (My_set.add v blocked)
			| App(x, y) ->
				My_set.union (all_free_vars x blocked) (all_free_vars y blocked) in

	let free_set = all_free_vars l1 My_set.empty in

	let rec impl free_set blocked_set l2 str = 
		match l2 with
		App(x, y)  -> (impl free_set blocked_set x str) &&  (impl free_set blocked_set y str)
		|Abs(x, y) -> if (x = str) then true else (impl free_set (My_set.add x blocked_set) y str)
		|Var(x)    -> if (x = str) then ((My_set.inter free_set blocked_set) = My_set.empty) else true in

	impl free_set My_set.empty l2 str;;


















