type algebraic_term = Var of string | Fun of string * (algebraic_term list)

(*val system_to_equation: (algebraic_term * algebraic_term) list -> (algebraic_term * algebraic_term)
*)

let rec list_to_string x str = match x with
         |[]-> str
         |h::t -> type_to_string h (" " ^  (list_to_string t str))



and type_to_string x str = match x with 
        |Var a -> a ^ " " ^ str
        |Fun(a, b) -> "(" ^ a ^ " " ^ (list_to_string b str) ^ ")" ;;


let rec system_to_string x str = match x with
        |[] -> str
        |(l,r)::t -> type_to_string l "" ^ "= " ^  type_to_string r "" ^ "\n" ^
        system_to_string t str;;


let print_system x = system_to_string x "";;

let rec go x str = match x with
        |[]-> str
        |h::t -> simple_name h (go t str) 

and  simple_name x str = match x with
        |Var a -> str ^ a
        |Fun (a, h) -> a ^ (go h str) ;;

let rec  get_name x str = match x with
        |[]-> str       
        |(l,r)::t -> simple_name l (simple_name r (get_name t str));;  

let get_fresh_name x = get_name x "fresh";;

let s0 = [(Var "a", Var "b");(Var "a", Var "b")];;
print_string(print_system s0);;


let rec eq_helper x ll rr = match x with 
        |[] -> ll, rr
        |(lh, rh)::t -> eq_helper t (lh::ll) (rh::rr);; 

let system_to_equation x = 
        let l, r = eq_helper x [] [] in
        let fresh_name = get_fresh_name x in
        (Fun(fresh_name, l), Fun(fresh_name, r));;

(*


let apply_substitution x y = failwith "Not implemented";;
let check_solution x y = failwith "Not implemented";;
let solve_system x = failwith "Not implemented";;*)
