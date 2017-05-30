(*val infer_simp_type : lambda -> ((string * simp_type list) * simp_type) option
*)

open Hw1
type simp_type = S_Elem of string | S_Arrow of simp_type * simp_type
let counter = ref 0 ;;

let name_generator () =
        let ret = "t"^ string_of_int !counter in                                              
        counter := !counter + 1; ret;;


let st_to_string st = 
	let rec impl st str = 
		match st with 
			S_Elem v -> str ^ v
			| S_Arrow (x, y) -> str ^ (impl x "") ^ " -> " ^ (impl y "") in
	impl st "";;


module Mmap = Map.Make (String);;

let rec stat x = match x with
        |S_Elem a -> Hw2_unify.Var a
        |S_Arrow (a, b) -> (Hw2_unify.Fun ("impl", [stat a; stat b]));;

let stas sys = 
        List.map (fun(a, b) -> (stat a, stat b)) sys;;

let [@warning "-8"] rec atst x = match x with 
        |Hw2_unify.Var a -> S_Elem a
        |(Hw2_unify.Fun (a, [b; c])) -> S_Arrow (atst b, atst c);;


let atss sys = 
        List.map (fun(a, b) -> (atst a, atst b)) sys;;

let sas_to_sss sys = 
        List.map (fun(a, b) -> (a, atst b)) sys;;


let list_to_map l =
        let rec helper l map = match l with
                |[] -> map
                |h::t -> helper t (Mmap.add h (S_Elem(name_generator())) map) in
        helper l Mmap.empty;;
        
        
let rec in_simp_helper xx map = match xx with
        |Var(x)    -> 
                        ([], Mmap.find x map)
        |App(x, y) ->
                        let sys1, type_t1 = in_simp_helper x map in
                        let sys2, type_t2 = in_simp_helper y map in
                        let tt = S_Elem(name_generator()) in
                        ((List.append sys1
                                (List.append sys2
                                        [(type_t1, S_Arrow (type_t2, tt))])), tt) 
        |Abs(x, y) -> 
                        let t = (Mmap.add x (S_Elem(name_generator())) map) in
                        let sys1, type_t1 = in_simp_helper y t in
                        (sys1, S_Arrow(Mmap.find x t, type_t1));;



let infer_simp_type x = 
        let sys, type_t =  in_simp_helper x (list_to_map
        (Hw1_reduction.free_vars x)) in
        match (Hw2_unify.solve_system (stas sys)) with  
                |None -> None
                |Some a -> Some (sas_to_sss a, atst
                (Hw2_unify.apply_substitution a (stat type_t)));;

(*
infer_simp_type(Hw1.lambda_of_string("\\x.x"));
*)



let None  = infer_simp_type (Hw1.lambda_of_string "\\x.x x");;
