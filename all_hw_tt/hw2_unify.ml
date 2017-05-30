type algebraic_term = Var of string | Fun of string * (algebraic_term list)

(*------------------- HELPER FUNCTIONS -----------------*)

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


let print_system x = print_string (system_to_string x "\n");;





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
(*
let s0 = [(Var "a", Var "b");(Var "a", Var "b")];;
print_string(print_system s0);;
*)
(*----------------------PART | --------------------------*)

let rec eq_helper x ll rr = match x with 
        |[] -> ll, rr
        |(lh, rh)::t -> eq_helper t (lh::ll) (rh::rr);; 

let system_to_equation x = 
        let l, r = eq_helper x [] [] in
        let fresh_name = get_fresh_name x in
        (Fun(fresh_name, l), Fun(fresh_name, r));;
let print_eq x str = print_system ( [x] )  
let print_equation x = print_eq x "";;
(*
print_string(print_equation(system_to_equation s0));;
*)
(*----------------------PART || -------------------------*)

module StringMap = Map.Make (String);;

let rec apply_helper_list y map = 
        let rec impl y z = match y with 
                |[] -> List.rev z
                |h::t -> impl t ((apply_helper h map)::z) in
        impl y []

and apply_helper y map  = match y with
        |Var a -> if StringMap.mem a map then StringMap.find a map else (Var a)
        |Fun(a, b) -> Fun(a, apply_helper_list b map);; 

let apply_substitution x y = 
        let rec fill_map l m = match l with 
                        [] -> m
                        |(ll, rr)::t -> fill_map t (StringMap.add ll rr m) in

        apply_helper y (fill_map x StringMap.empty);;

(*----------------------PART ||| -------------------------*)

let rec list_check_helper x y res = match y with
        |[] -> res
        |(l,r)::t ->list_check_helper x t (res
                && ((type_to_string(apply_substitution x l) "") =
                        (type_to_string(apply_substitution x r) "")));;


let check_solution x y = list_check_helper x y true;;

(*
let at0 = Fun("f",[Fun("g",[Var "y"; Var "x"])]);;
let s1 = [(Var "a", Var "b");(Var "x", Var "b")];;
print_string(string_of_bool(check_solution ["a", Var"b"; "x", Var"z"] s1));;
*)

(*----------------------PART |V --------------------------*)

exception OOps of string;;
module StringSet  = Set.Make (String);;

let rec list_contains x str = match x with
         |[]-> false
         |h::t -> type_contains h str ||  list_contains t str

and type_contains x str = match x with
        |Var a -> a = str
        |Fun(a, b) -> list_contains b str;;

let [@warning "-8"] rec make_solutions_list b d res = match (b, d) with
        |([], c) -> res
        |(bh::bt, dh::dt) -> make_solutions_list bt dt ((bh, dh)::res);;

let rec system_subst subst x res = match x with
        |[] -> List.rev res
        |(l,r)::t -> system_subst subst t 
        (((apply_substitution subst l), (apply_substitution subst r))::res);; 

let [@warning "-8"] rec var_reduction system res = match system with
        |[] -> List.rev res
        |(Var l, r)::t -> var_reduction t ((l, r)::res);;

let solve system l r set = match (l, r) with
        |(Var a, b) ->
                if (type_contains b a) 
                        then raise (OOps ("no solution :("))
                else let set = StringSet.add a set in
                        ((List.append
                        (system_subst [a,b] system []) [l,r]), set)
                                
                        
        |(a, Var b) -> ((List.append system [Var b, a]), set) 
        |(Fun(a,b), Fun(c,d)) -> 
                if (a <> c || List.length b <> List.length d) 
                        then raise (OOps ("no solution :("))
                else 
                        (List.append system (make_solutions_list b d []), set);;

let solve_system x =
        try
                let rec solve_helper x set  = match x with
                         |[] -> x
                         |(l,r)::t ->
                                if (type_to_string (l) "" = type_to_string (r) "")
                                        then solve_helper t set
                                else 
                                        let new_list, new_set = (solve t l r set) in
                                        if ((List.length new_list) = StringSet.cardinal new_set) 
                                                then new_list
                                        else 
                                                solve_helper new_list new_set
                in let result =  solve_helper x StringSet.empty
                in print_string (system_to_string result "");
                (Some (var_reduction result []))
        with (OOps what)  -> (print_string (what ^"\n"));
                None;;


let at0 = Var "a";;
let at1 = Var "b";;
let at2 = Var "c";;
let at3 = Var "d";;
let at4 = Fun("f",[Var "x"]);;
let at5 = Fun("f",[Fun("g",[Var "y"])]);;
let at6 = Fun("h",[Var "p"]);;
let at7 = Fun("f",[Var "a"; Var "b"]);;
let at8 = Fun("f",[Var "x"; Var "y"]);;
let at9 = Fun("f",[at5; at7]);;

let sys0 = [(Var "a", Var "b"); (Var "c", Var "d")];;
let sys1 = [(Fun("f",[Var "x"]), Fun("f",[Fun("g",[Var "y"])])); (Var "y", Fun("h",[Var "p"]))];;
let sys2 = [(Fun("f",[Var "a"]), Var "b")];;
let sys3 = [Fun("f",[Var "a"; Var "b"]), Fun("f",[Var "x"; Var "y"])];;

let isys0 = [at4, at8];;
let isys1 = [Fun("f",[Var "y"; Fun("h",[Var "x"; Var "y"])]), Fun("f",[Fun("g",[Var "a"; Var "b"]); Fun("h", [Var "x"; Var "x"])]); Fun("h",[Var "x"; Var "y"]), Fun("h", [Var "a"; Var "a"])];;

(*print_system isys1;;
solve_system isys1;;
*)






