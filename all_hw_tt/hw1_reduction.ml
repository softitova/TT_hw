
open Hw1

let rec subst lambda oldVar newVar = 
	match lambda with 
	|Var(x)    -> if (x = oldVar) then Var(newVar) else Var(x)
	|App(x, y) -> App(subst x oldVar newVar, subst y oldVar newVar) 
	|Abs(x, y) -> if (x = oldVar) then Abs(x, y) else Abs(x, subst y oldVar newVar);;

let is_alpha_equivalent x y = 

	let counter = ref 0 in
	
	let name_generation () =
        	let ret = "t"^ string_of_int !counter in
       		counter := !counter + 1;
  		ret in

	let rec make x y = match (x, y) with
		|(Var xx, Var yy) -> (xx = yy)
		|(App(x1, x2), App(y1, y2)) -> ((make x1 y1) && (make  x2  y2))
		|(Abs(x1, x2), Abs(y1, y2)) -> 
			let next_free_character = (name_generation()) in
			make (subst x2 x1 next_free_character) (subst y2 y1 next_free_character)
		|_ -> false in

	make x y;;

let rec free lambda str  = 
	match lambda with
	|Var(x)    -> true 
	|App(x, y) -> (free x str) && (free y str)
	|Abs(x, y) -> if (x = str) then false  else free y str;;

(*------------------ FREE VARS ------------------*)

module My_set = Set.Make (String);;

let rec helper l blocked = match l with
                |Var v     -> if My_set.mem v blocked then My_set.empty else My_set.singleton v
                |Abs(v, x) -> helper x (My_set.add v blocked)
                |App(x, y) -> My_set.union (helper x blocked) (helper y blocked);;

let free_vars l = My_set.elements (helper l My_set.empty);;
       

(*------------------ FREE TO SUBST ------------------*)

let free_to_subst l1 l2 str = 
	
	let free_set = helper l1 My_set.empty in

	let rec impl free_set blocked_set l2 str = 
		match l2 with
                |App(x, y) -> (impl free_set blocked_set x str) &&  (impl free_set blocked_set y str)
		|Abs(x, y) -> if (x = str) then true else (impl free_set (My_set.add x blocked_set) y str)
		|Var(x)    -> if (x = str) then ((My_set.inter free_set blocked_set) = My_set.empty) else true in

	impl free_set My_set.empty l2 str;;



(*------------------ IS NORMAL FORM ------------------*)

let rec check_normal xx res = match xx with
        |Var x -> true && res
        |App(Abs(l, r), y) -> false
        |App(x, y) -> (check_normal x res) && (check_normal y res)
        |Abs(x, y) -> check_normal y res;;
        

let is_normal_form x = check_normal x true;;


(*------------------ NORMAL B REDUCTION ------------------*)
let counter = ref 0;;

let name_gen () =
        let ret = "t"^ string_of_int !counter in
        counter := !counter + 1; ret;;


let rec subst_term lambda oldVar newVar = 
	match lambda with 
	|Var(x)    -> if (x = oldVar) then newVar else Var(x)
	|App(x, y) -> App(subst_term x oldVar newVar, subst_term y oldVar newVar) 
	|Abs(x, y) -> if (x = oldVar) then Abs(x, y) else Abs(x, subst_term y oldVar newVar);;

module StringMap = Map.Make(String);;



let rec rename lambda = match lambda with 
        |Abs(x, y) ->(let fr = (name_gen()) in
                Abs(fr, subst_term (rename y)  x (Var(fr))))
        |App(x, y) -> App((rename x), (rename y))
        |Var(x) -> Var(x);;
print_string(string_of_lambda(rename (lambda_of_string "(\\f.\\x.x)"))^"\n");;

let rec beta_reduction_step xx res = match xx with
        |Var x -> (Var x, res || false)
        |App(Abs(l, r), y) ->  (
                 (subst_term r l (rename  y), true))
        |App(x, y) -> let l1, r1= beta_reduction_step x res in
                        if (r1 = false) 
                        then let l2, r2 = beta_reduction_step y res in
                        (App(l1, l2), r2)
                        else (App(l1, y), true)
        |Abs(x, y) -> let l1 , r1 = beta_reduction_step y res in
                        (Abs(x, l1), r1);;

let normal_beta_reduction x = let l, r = beta_reduction_step x false in l;;

(*------------------ REDUCE TO NORMAL FORM ------------------*)

let rec reduce_to_normal_form x = if (is_normal_form x) then x else reduce_to_normal_form (normal_beta_reduction x);;
print_string (string_of_lambda (reduce_to_normal_form (lambda_of_string
"(\\f.\\x.f (f x)) (\\f.\\x.f (f x))")));;
(* print_string(string_of_lambda(reduce_to_normal_form(lambda_of_string"((\\f.(\\x.f (x x)) (\\x.f (x x))) (\\f.\\n.(\\n.n (\\x.\\x.\\y.y) \\x.\\y.x) n (\\f.\\x.f x) ((\\a.\\b.a ((\\a.\\b.\\f.\\x.a f (b f x)) b) \\f.\\x.x) n (f ((\\n.(\\p.p \\x.\\y.x) (n (\\p.\\f.f ((\\p.p \\x.\\y.y) p) ((\\n.\\f.\\x.f (n f x)) ((\\p.p \\x.\\y.y) p))) (\\f.f (\\f.\\x.x) (\\f.\\x.x)))) n))))) \\f.\\x.f (f (f (f (f (f x)))))"))^"\n");; *)




