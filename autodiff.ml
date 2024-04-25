open Ast

exception AutodiffError of string * Lexing.position

let check ast =
  let scalar = DimInt [1] in
	let vars = Hashtbl.create 100 in
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
      | DimInt (1::dims) -> DimInt (len :: dims)
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

type offset_type = Buffer of int | Input | BPInput

let rec build_forward_expression ofile vars vars_offset input_dim offset (e, _) :
    int * int list * offset_type expr =
  let build_forward_expression = build_forward_expression
    ofile vars vars_offset input_dim in
  match e with
  | Int x -> (
    Printf.fprintf ofile " buffer[%d] = %d;\n" offset x;
    offset + 1, [], (Int x, Buffer offset)
  )
  | Float x -> (
    Printf.fprintf ofile " buffer[%d] = %f;\n" offset x;
    offset + 1, [], (Float x, Buffer offset)
  )
  | Vec (e, len) -> (
    let new_offset, dims, e = build_forward_expression offset e in
    let size = (new_offset - offset) in

    for i=1 to len do
      Printf.fprintf ofile " memcpy(buffer + %d, buffer + %d, %d * sizeof(double));\n"
        (offset + size * i) (offset) (size);
    done;
    new_offset + ((len - 1) * size), len::dims, (Vec (e, len), Buffer new_offset)
  )
  | Var n when String.equal n "input" -> (
    let size = List.fold_left ( * ) 1 input_dim in
    Printf.fprintf ofile " memcpy(buffer + %d, input, %d * sizeof(double));\n"
      offset size;
    offset + size, input_dim, (Var n, Input)
  )
  | Var n -> (
    let dims, _ = Hashtbl.find vars n in
    match dims with
    | DimInt dims -> (
      let size = List.fold_left ( * ) 1 dims in
      let src_offset = Hashtbl.find vars_offset n in
      Printf.fprintf ofile " memcpy(buffer + %d, buffer + %d, %d * sizeof(double));\n"
        offset src_offset size;
      offset + size, dims, (Var n, Buffer offset)
    )
    | _ -> failwith "Impossible"
  )
  | Binop (op, lhs, rhs) -> (
    let lhs_offset = offset in
    let rhs_offset, ldims, lhs = build_forward_expression lhs_offset lhs in
    let offset, rdims, rhs     = build_forward_expression rhs_offset rhs in

    let rsize = List.fold_left ( * ) 1 rdims in
    let lsize = List.fold_left ( * ) 1 ldims in
    let size, dims =
      if rsize > lsize
      then rsize, rdims
      else lsize, ldims
      in
    Printf.fprintf ofile " for(i = 0; i < %d; i ++)\n" size;
    Printf.fprintf ofile "  buffer[%d + i] = %s %s %s;\n"
      offset
      (
        if ldims = []
        then "buffer[" ^ (string_of_int lhs_offset) ^ "]"
        else "buffer[" ^ (string_of_int lhs_offset) ^ " + i]"
      )
      (
        match op with
        | Add -> "+" | Sub -> "-"
        | Mul -> "*" | Div -> "/"
      )
      (
        if rdims = []
        then "buffer[" ^ (string_of_int rhs_offset) ^ "]"
        else "buffer[" ^ (string_of_int rhs_offset) ^ " + i]"
      );

    offset + size, dims, (Binop(op, lhs, rhs), Buffer offset)
  )

let build_forward_function ofile ast vars input_dim :
    int * (string, int) Hashtbl.t * offset_type program * string =
begin
  let var_backpropagation_start = ref "" in
  Printf.fprintf ofile
    "double* model_run(double *buffer, double *input) {\n int i;\n";
  let vars_offset = Hashtbl.create 100 in
  let buf_size, prg_with_offset = List.fold_left (fun (offset, prg) ((s, pos) : Lexing.position stmt) ->
    Printf.fprintf ofile " //Line %d; Column %d\n" pos.pos_lnum  (pos.pos_cnum-pos.pos_bol+1);
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
      ) offset l, prg
    )
    | SReturn n -> (
      var_backpropagation_start := n;
      Printf.fprintf ofile
        " return buffer + %d;\n" offset;
      (offset, prg)
    )
    | SVarDecl ((_, Unknown), _) -> failwith "Impossible"
    | SVarDecl ((name, DimInt dims), None) -> (
        let size = List.fold_left ( * ) 1 dims in
        Hashtbl.add vars_offset name offset; 
        (offset + size, prg)
    )
    | SVarDecl ((name, DimInt dims), Some e) -> (
      if not (Hashtbl.mem vars_offset name)
      then Hashtbl.add vars_offset name offset;
      let size = List.fold_left ( * ) 1 dims in
      let new_offset = offset + size in
      let out_offset, out_dims, out_expr =
        build_forward_expression ofile vars vars_offset input_dim new_offset e in
      (out_offset, (SVarDecl ((name, DimInt dims), Some out_expr), Buffer offset)::prg)
    )
  ) (0, []) ast in
  Printf.fprintf ofile "}\n%!";
  buf_size, vars_offset, List.rev prg_with_offset, !var_backpropagation_start
end

let add_missing_dimensions vars ast =
    let get_dims (name : string) : param_dims =
        let dims, _ = Hashtbl.find vars name in dims
        in
    let aux_stmt s =
        let s, pos = s in (
          match s with
          | SVarDecl ((name, Unknown), x) -> (
              SVarDecl ((name, get_dims name), x)
          )
          | SVarDecl ((name, DimInt dims), x) -> (
              SVarDecl ((name, DimInt dims), x)
          )
          | SParamDecl args -> (
              SParamDecl (
                  List.map (fun (name, _) -> (name, get_dims name)) args
              )
          )
          | SReturn s -> SReturn s
        ), pos
        in
    List.map aux_stmt ast

type bp_stack_elt =
  | OpInt of binop * int
  | OpFloat of binop * float
  | OpOffset of binop * offset_type
  | Start of offset_type
  | SumElements of (int * int)

let build_backward_propagation (ast : offset_type program) (start_var : string) vars_to_bp vars vars_offset ofile =
  let vars_bp_stack = Hashtbl.create (Hashtbl.length vars) in
  Hashtbl.iter (fun name _ -> Hashtbl.add vars_bp_stack name []) vars;
  Hashtbl.replace vars_bp_stack start_var [[Start BPInput]];
  let rec aux_expr bp_operation_stack dims (e, offset) = match e with
    | Int _ -> ()
    | Float _ -> ()
    | Var name -> (
      let current_op_stack = Hashtbl.find vars_bp_stack name in
      Hashtbl.replace vars_bp_stack name (bp_operation_stack::current_op_stack)
    )
    | Binop(Add, lhs, rhs) -> (
      (* d(a+b)/da = 1 *)
      aux_expr bp_operation_stack dims lhs;
      (* d(a+b)/db = 1 *)
      aux_expr bp_operation_stack dims rhs;
    )
    | Binop(Sub, lhs, rhs) -> (
      (* d(a-b)/da = 1 *)
      aux_expr bp_operation_stack dims lhs;
      (* d(a-b)/db = -1 *)
      aux_expr ((OpInt (Mul, -1))::bp_operation_stack) dims rhs;
    )
    | Binop(Mul, (lhs, lhs_offset), (rhs, rhs_offset)) -> (
      (* d(a*b)/da = b *)
      aux_expr ((OpOffset (Mul, rhs_offset))::bp_operation_stack) dims (lhs, lhs_offset);
      (* d(a*b)/da = a *)
      aux_expr ((OpOffset (Mul, lhs_offset))::bp_operation_stack) dims (rhs, rhs_offset);
    )
    | Binop(Div, (lhs, lhs_offset), (rhs, rhs_offset)) -> (
      (* d(a/b)/da = 1/b *)
      aux_expr ((OpOffset (Div, rhs_offset))::bp_operation_stack) dims (lhs, lhs_offset);
      (* d(a/b)/db = -a/b^2 = -((a/b)/b) *)
      aux_expr (
        (OpInt (Mul, -1))::
        (OpOffset (Div, rhs_offset))::
        (OpOffset (Div, rhs_offset))::
        (OpOffset (Mul, lhs_offset))::
        bp_operation_stack
      ) dims (rhs, rhs_offset);
    )
    | Vec(e, count) -> (
      let dims = List.tl dims in
      aux_expr 
        (
          (SumElements (count, (List.fold_left ( * ) 1 dims)))::
          bp_operation_stack
        )
        dims e;
    )
    in

  List.iter (fun (s, offset) ->
    match s with
    | SVarDecl (_, None) -> ()
    | SParamDecl _ -> ()
    | SVarDecl ((_, Unknown), _) -> failwith "Impossible"
    (* This case is impossible because build_forward_function removes
      the returns *)
    | SReturn _ -> failwith "Impossible"
    | SVarDecl ((name, DimInt dims), Some e) -> (
      let previous_bp_operations = Hashtbl.find vars_bp_stack name in
      List.iter
        (fun prev_op -> aux_expr prev_op dims e)
        previous_bp_operations
    )
  ) (List.rev ast);
  
  let binop_to_string b = match b with
    | Add -> "+" | Sub -> "-"
    | Mul -> "*" | Div -> "/"
  in
  Printf.fprintf ofile "void model_train(double *buffer, double *bp_buffer, double *input, double *bp_input, double learning_rate) {\n int i, j;\n";
  let start_dims =
    match Hashtbl.find vars start_var with
    | DimInt d, _ -> d
    | _ -> failwith "Impossible"
    in
  let start_size = List.fold_left ( * ) 1 start_dims in
  let return = List.fold_left (fun bp_buf_size name ->
      Printf.fprintf ofile " //%s\n" name;
      let ops = Hashtbl.find vars_bp_stack name in
      let dims =
        match Hashtbl.find vars name with
        | DimInt d, _ -> d
        | _ -> failwith "Impossible"
        in
      let param_size = List.fold_left ( * ) 1 dims in
      List.iter (fun op_stack ->
        ignore(List.fold_left (fun dims op ->
          let size = List.fold_left ( * ) 1 dims in
          match op with
          | OpInt (b, x) -> (
            Printf.fprintf ofile " for(i = 0; i < %d; i ++)\n" size;
            Printf.fprintf ofile "  bp_buffer[i + %d] %s= %d;\n" bp_buf_size (binop_to_string b) x;
            
            dims
          )
          | OpFloat (b, x) -> (
            Printf.fprintf ofile " for(i = 0; i < %d; i ++)\n" size;
            Printf.fprintf ofile "  bp_buffer[i + %d] %s= %f;\n" bp_buf_size (binop_to_string b) x;
            
            dims
          )
          | OpOffset (b, Input) -> (
            Printf.fprintf ofile " for(i = 0; i < %d; i ++)\n" size;
            Printf.fprintf ofile "  bp_buffer[i + %d] %s= input[i];\n" bp_buf_size (binop_to_string b);
            
            dims
          )
          | OpOffset (b, Buffer o) ->  (
            Printf.fprintf ofile " for(i = 0; i < %d; i ++)\n" size;
            Printf.fprintf ofile "  bp_buffer[i + %d] %s= buffer[i + %d];\n" bp_buf_size (binop_to_string b) o;
            
            dims
          )
          | OpOffset (b, BPInput) -> (
            Printf.fprintf ofile " for(i = 0; i < %d; i ++)\n" size;
            Printf.fprintf ofile "  bp_buffer[i + %d] %s= bp_input[i];\n" bp_buf_size (binop_to_string b);
            
            dims
          )
          | SumElements (count, offset) -> (
            Printf.fprintf ofile " for(j = 0; j < %d; j ++)\n" count;
            Printf.fprintf ofile "  for(i = 1; i < %d; i ++)\n" (offset+1);
            Printf.fprintf ofile "   bp_buffer[j * %d + %d] += bp_buffer[i + j * %d + %d];\n"
              offset bp_buf_size offset bp_buf_size;

            Printf.fprintf ofile " for(j = 1; j < %d; j ++)\n" count;
            Printf.fprintf ofile "   bp_buffer[j + %d] = bp_buffer[j * %d + %d];\n"
              bp_buf_size offset bp_buf_size;
            let new_dims = match dims with
              | [] -> []
              | _::[] -> [1]
              | _::q -> q
              in
            new_dims
          )
          | Start BPInput -> (
            Printf.fprintf ofile " memcpy(bp_buffer + %d, bp_input, %d * sizeof(double));\n"
              bp_buf_size start_size;
            start_dims
          )
          | Start _ -> failwith "Impossible"
        ) [] (List.rev op_stack));

        let param_offset = Hashtbl.find vars_offset name in

        Printf.fprintf ofile " for(i = 0; i < %d; i ++)\n" param_size;
        Printf.fprintf ofile "  buffer[i + %d] -= bp_buffer[i + %d] * learning_rate;\n" param_offset bp_buf_size
      ) ops;
      bp_buf_size + start_size
    ) 0 vars_to_bp
    in
  Printf.fprintf ofile "}\n";
  return

let build_program ast ofile =
(* - Construction du programme *)
  (* Check the program & retrieve the variables *)
	let vars = check ast in
  let ast = add_missing_dimensions vars ast in
	let parameters = ref [] in
	let input_dim, _ = Hashtbl.find vars "input" in
	
	Hashtbl.iter (fun name (dims, is_param) ->
		if is_param
		then parameters := (name, dims) :: !parameters
		else ()
	) vars;

  let parameters = !parameters in

  (* Build the forward propagation *)
  Printf.fprintf ofile "#include <string.h>\n";
  Printf.fprintf ofile "#include <stdlib.h>\n";

  let buf_size, vars_offset, ast, start =
    match input_dim with
    | DimInt input_dim -> build_forward_function ofile ast vars input_dim
    | _ -> failwith "Impossible"
  in

  (* Build the backpropagation function *)
  let bp_buf_size = build_backward_propagation
    ast start
    (List.map (fun (n, _) -> n) parameters)
    vars vars_offset ofile
    in

  Printf.fprintf ofile
    "void model_init(double **fp_buf, double **bp_buf) {\n\
    \ *fp_buf = malloc(%d * sizeof(double));\n\
    \ *bp_buf = malloc(%d * sizeof(double));\n\
    }\n"
    buf_size
    bp_buf_size