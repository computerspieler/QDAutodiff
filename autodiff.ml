open Ast
open Graph

exception AutodiffError of string * Lexing.position

let check ast =
	let vars = Hashtbl.create 100 in
  let scalar = DimInt [1] in
	(* La première valeur contient les dimension 
	   La deuxième détermine si cette variable est un
	   paramètre *)
	Hashtbl.add vars "input" (Unknown, false);

	let check_dimensions dims dims' =
		match dims, dims' with
		| DimInt l1, DimInt l2 -> (List.equal (=) l1 l2)
		| DimInt _, Unknown -> false
		| Unknown, DimInt _ -> false
		| Unknown, Unknown -> false
		in

	let add_variable pos name dims is_param =
		match Hashtbl.find_opt vars name with
		| None -> Hashtbl.add vars name (dims, is_param)
		| Some (dims', is_param') -> (
			if is_param && not is_param'
			then raise (AutodiffError ("A variable can't be turned into a parameter", pos));
			if not is_param && is_param'
			then raise (AutodiffError ("A parameter can't be turned into a variable", pos));

			if not (check_dimensions dims dims') && dims <> Unknown && dims' <> Unknown
			then raise (AutodiffError ("Incompatible dimensions", pos));

			Hashtbl.replace vars name (
				(if dims <> Unknown then dims else dims'),
				is_param
			)
		)
		in

	let rec check_expr_dims e =
    let e, pos = e in
    match e with
		| Int _
		| Float _ -> scalar
		| Var n -> (
			match Hashtbl.find_opt vars n with
			| Some (dims, _) -> dims
			| None -> raise (AutodiffError ("Use of undefined variable " ^ n, pos))
		) 
		| Binop (MatMul, mat, vec) -> (
			match check_expr_dims mat, check_expr_dims vec with
			| Unknown, _ -> raise (AutodiffError ("Invalid dimension for the LHS", pos))
			| _, Unknown -> raise (AutodiffError ("Invalid dimension for the RHS", pos))
			| DimInt mat, DimInt vec -> (
				let rec aux2 mat vec =
					match mat, vec with
					| [tm1], [tv1] when tv1 = tm1 -> []
					| [tm2; tm1], [tv1] when tv1 = tm1 -> [tm2]
					| t1::q1, t2::q2 when t1 = t2 -> t1::(aux2 q1 q2)
					| _, _ -> raise (AutodiffError ("Incompatible dimensions", pos))
				in DimInt (aux2 mat vec)
			)
		)
		| Binop (o, e1, e2) -> (
			let d1 = check_expr_dims e1 in
			let d2 = check_expr_dims e2 in

			if o = Mul || o = Div
			then (
				if d1 = scalar then d2
				else if d2 = scalar then d1
				else
					if check_dimensions d1 d2
					then d1
					else raise (AutodiffError ("Incompatible dimensions", pos));
			) else (
				if check_dimensions d1 d2
				then d1
				else raise (AutodiffError ("Incompatible dimensions", pos))
			)
		)
		in

	let check_stmt s =
    let s, pos = s in
		match s with
		| SParamDecl v -> (
			List.iter (fun (name, dims) ->
				add_variable pos name dims true
			) v
		)

		| SReturn s -> (
			match Hashtbl.find_opt vars s with
			| None -> raise (AutodiffError ("Unknown output", pos))
			| Some (Unknown, _) -> raise (AutodiffError ("Unknown output dimension", pos))
			| Some (_, true) -> raise (AutodiffError ("The output can't be a parameter", pos))
			| Some _ -> ()
		)

		| SVarDecl ((name, dims), None) -> (
			add_variable pos name dims false
		)

		| SVarDecl ((name, dims), Some e) -> (
			let dims' = check_expr_dims e in
			match dims, dims' with
			| _, Unknown -> raise (AutodiffError ("Unknown dimension for expression", pos))
			| Unknown, _ -> (add_variable pos name dims' false)
			| _, _ -> (
				if not (check_dimensions dims dims')
				then raise (AutodiffError ("Incompatible dimensions", pos));

				add_variable pos name dims false
			);
		)
		in
	let rec aux ast =
		match ast with
		| [] -> ()
		| t::q -> (
			check_stmt t;
			aux q
		)
		in
	aux ast; vars

let build_dependancy_graph ast vars =
	let output = graph_init
		(Hashtbl.to_seq vars
			|> Array.of_seq
			|> Array.map (fun (x, _) -> x)
		) in
	let out_var = ref (-1) in
	let get_graph_index s =
		let out_index = ref (-1) in
		Array.iteri (fun i s' -> if s = s' then out_index := i) output.vtx;
		!out_index
		in
	let rec aux_expr src_i (e, _) =
    match e with
		| Int _ | Float _ -> ()
		| Var dst -> (
			let dst_i = get_graph_index dst in
			if src_i <> dst_i
			then ignore (ajout_arete output dst_i src_i)
			else ()
		)
		| Binop(_, l, r) -> (
			aux_expr src_i r;
			aux_expr src_i l
		)
		in
	let aux_stmt (s, _) = match s with
		| SParamDecl _ -> ()
		| SVarDecl (_, None) -> ()
		| SReturn s -> out_var := get_graph_index s
		| SVarDecl ((name, _), Some e) -> aux_expr (get_graph_index name) e
		in
	List.iter aux_stmt ast;
	output, !out_var

let build_program ast _ofile =
(* - Construction du programme *)
	let vars = check ast in
	let parameters = ref [] in
	let temp_vars = ref [] in
	let input_dim, _ = Hashtbl.find vars "input" in
	
	Hashtbl.iter (fun name (dims, is_param) ->
		if is_param
		then parameters := (name, dims) :: !parameters
		else if name <> "input"
		then temp_vars := (name, dims) :: !temp_vars
		else ()
	) vars;

	let g, start = build_dependancy_graph ast vars in
	let visit_order = topo_sort g in
	List.iter (fun i ->
    let name = g.vtx.(i) in
    let _, is_param = Hashtbl.find vars name in
    if is_param
    then Printf.printf "%s, " name;
  ) (List.rev visit_order);
	print_newline ();

	let print_var (name, dims) =
		Printf.printf "%s: (%s)\n"
			name (match dims with
				| Unknown -> "unknown"
				| DimInt l -> (List.fold_left (fun acc x -> acc ^ (string_of_int x) ^ ",") "" l)
			);
		in
		
	print_var ("Input size", input_dim);
	Printf.printf "=== Parameters ===\n";
	List.iter print_var !parameters;
	Printf.printf "=== Variables ===\n";
	List.iter print_var !temp_vars
