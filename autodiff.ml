open Ast
open Graph

exception AutodiffError of string * Lexing.position

let check ast =
	let vars = Hashtbl.create 100 in
  let scalar = DimInt [] in
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
    | Vec (e, len) -> (
      match check_expr_dims e with
      | DimInt dims -> DimInt (len :: dims)
      | Unknown -> Unknown
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
      (* Extract the name *)
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
    | Vec (e, _) -> aux_expr src_i e
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

let rec build_forward_expression ofile vars vars_offset input_dim offset (e, _) =
  let build_forward_expression = build_forward_expression
    ofile vars vars_offset input_dim in
  match e with
  | Int x -> (
    Printf.fprintf ofile " buffer[%d] = %d;\n" offset x;
    offset + 1, []
  )
  | Float x -> (
    Printf.fprintf ofile " buffer[%d] = %f;\n" offset x;
    offset + 1, []
  )
  | Vec (e, len) -> (
    let new_offset, dims = build_forward_expression offset e in
    let size = (new_offset - offset) in

    for i=1 to len do
      Printf.fprintf ofile " memcpy(buffer + %d, buffer + %d, %d * sizeof(double));\n"
        (offset + size * i) (offset) (size);
    done;
    new_offset + ((len - 1) * size), len::dims
  )
  | Var n when String.equal n "input" -> (
    let size = List.fold_left ( * ) 1 input_dim in
    Printf.fprintf ofile " memcpy(buffer + %d, input, %d * sizeof(double));\n"
      offset size;
    offset + size, input_dim
  )
  | Var n -> (
    let dims, _ = Hashtbl.find vars n in
    match dims with
    | DimInt dims -> (
      let size = List.fold_left ( * ) 1 dims in
      let src_offset = Hashtbl.find vars_offset n in
      Printf.fprintf ofile " memcpy(buffer + %d, buffer + %d, %d * sizeof(double));\n"
        offset src_offset size;
      offset + size, dims
    )
    | _ -> failwith "Impossible"
  )
  | Binop (op, lhs, rhs) -> (
    let lhs_offset    = offset in
    let rhs_offset, _ = build_forward_expression lhs_offset lhs in
    let offset, dims  = build_forward_expression rhs_offset rhs in

    let size = List.fold_left ( * ) 1 dims in
    Printf.fprintf ofile " for(int i = 0; i < %d; i ++)\n" size;
    Printf.fprintf ofile (
      match op with
      | Add -> "  buffer[%d + i] = buffer[%d + i] + buffer[%d + i];\n"
      | Sub -> "  buffer[%d + i] = buffer[%d + i] - buffer[%d + i];\n"
      | Mul -> "  buffer[%d + i] = buffer[%d + i] * buffer[%d + i];\n"
      | Div -> "  buffer[%d + i] = buffer[%d + i] / buffer[%d + i];\n"
    ) offset lhs_offset rhs_offset;

    offset + size, dims
  )

let build_forward_function ofile ast vars input_dim = begin
  Printf.fprintf ofile
    "double* model_run(double *buffer, double *input) {\n";
  let vars_offset = Hashtbl.create 100 in
  let buf_size = List.fold_left (fun offset (s, pos) ->
    match s with
    | SParamDecl l -> (
      List.fold_left (fun offset (n, dims) ->
        match dims with
        | DimInt dims -> (
          let size = List.fold_left ( * ) 1 dims in
          Hashtbl.add vars_offset n offset; 
          offset + size
        )
        | _ -> failwith "Impossible"
      ) offset l
    )
    | SReturn n -> (
      Printf.fprintf ofile
        " return buffer + %d;\n" offset;
        offset
    )
    | SVarDecl (_, None) -> offset
    | SVarDecl ((name, _), Some e) ->
      Hashtbl.add vars_offset name offset;
      let offset, _ = build_forward_expression ofile vars vars_offset input_dim offset e in
      offset
  ) 0 ast in
  Printf.fprintf ofile
    "}\n%!";
end

let build_program ast ofile =
(* - Construction du programme *)
  (* Check the program & retrieve the variables *)
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

  let parameters = Array.of_list !parameters in
  let temp_vars = Array.of_list !temp_vars in

  (* Build the forward propagation *)
  match input_dim with
  | DimInt input_dim -> build_forward_function ofile ast vars input_dim
  | _ -> ();

  (* Get the backpropagation order *)
	let g, start = build_dependancy_graph ast vars in
	let visit_order = topo_sort g in
	List.iter (fun i ->
    let name = g.vtx.(i) in
    let _, is_param = Hashtbl.find vars name in
    if is_param
    then Printf.printf "%s, " name;
  ) (List.rev visit_order);
	print_newline ();

  (* Print the variables dimensions *)
	let print_var (name, dims) =
		Printf.printf "%s: (%s)\n"
			name (match dims with
				| Unknown -> "unknown"
				| DimInt l -> (List.fold_left (fun acc x -> acc ^ (string_of_int x) ^ ",") "" l)
			);
		in
		
	print_var ("Input size", input_dim);
	Printf.printf "=== Parameters ===\n";
	Array.iteri (fun i v ->
    print_var v;
  ) parameters;
	Printf.printf "=== Variables ===\n";
	Array.iter print_var temp_vars
