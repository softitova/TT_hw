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
let print_eq x str = print_system ( [x] )  
let print_equation x = print_eq x "";;

print_string(print_equation(system_to_equation s0));;

(*----------------------PART || -------------------------*)
module StringMap = Map.Make (String);;

let rec apply_helper_list y map = 
        let rec impl y z = match y with 
                |[] -> z
                |h::t -> impl t ((apply_helper h map)::z) in
        impl y []

and apply_helper y map  = match y with
        |Var a -> if StringMap.mem a map then StringMap.find a map else (Var a)
        |Fun(a, b) -> Fun(a, apply_helper_list b map);; 

let apply_substitution subst at = 
        let rec fill_map l m =
                match l with 
                        [] -> m
                        |(var, term)::t -> fill_map t (StringMap.add var term m) in

        apply_helper at (fill_map subst StringMap.empty);;


(*let check_solution x y = failwith "Not implemented";;
let solve_system x = failwith "Not implemented";;*)
