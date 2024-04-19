type 'a graph = {
    vtx : 'a array;
    edges : int list array;
}

let graph_init vtx =
    {
        vtx = vtx;
        edges = Array.make (Array.length vtx) [];
    }

let ajout_arete g i1 i2 =
    g.edges.(i1) <- i2::g.edges.(i1); g

let restriction g p =
    {
		vtx = Array.sub g.vtx 0 p;
		edges = Array.init p (fun i -> List.filter (fun x -> x < p) g.edges.(i))
	}

let par_defaut g =
	{
		vtx = g.vtx;
		edges = Array.mapi (fun i x ->
			List.filter (fun y -> List.mem i g.edges.(y)) x
		) g.edges
	}

let par_exces g =
	let out_edges = Array.copy g.edges in
	Array.iteri (fun i lx ->
		List.iter (fun y ->
			if not (List.mem i out_edges.(y))
			then out_edges.(y) <- i::out_edges.(y);
		) lx
	) out_edges;
	{
		vtx = g.vtx;
		edges = out_edges
	}

let miroir g =
	let out_edges = Array.make (Array.length g.edges) [] in
	Array.iteri (fun i lx ->
		List.iter (fun y -> 
			out_edges.(y) <- i::out_edges.(y)
		) lx
	) g.edges;
	{
		vtx = g.vtx;
		edges = out_edges
	}

let print_graph g =
	Printf.printf "Sommets [";
	Array.iter (fun x -> Printf.printf "%s; " x) g.vtx;
	Printf.printf "]\nArretes [|\n";
	Array.iteri (fun x lx ->
		Printf.printf "%d: [" x;
		List.iter (fun x -> Printf.printf "%d; " x) lx;
		Printf.printf "]\n"
	) g.edges;
	Printf.printf "|]\n%!"

let stat_degre g =
	let deg_max = ref 0 in
	let deg_sum = ref 0 in
	Array.iter (fun lx ->
		let d = List.length lx in
		if d > !deg_max then deg_max := d;
		deg_sum := !deg_sum + d;
	) g.edges;
	(!deg_max, (Float.of_int !deg_sum) /. (Float.of_int (Array.length g.vtx)))

let bfs g root =
	let dist = Array.make (Array.length g.vtx) None in
	let parent = Array.make (Array.length g.vtx) None in
	let q = Queue.create() in
  let visit_order = ref [] in

	Queue.add (0, root) q;
	parent.(root) <- Some (-1);
	while not (Queue.is_empty q) do
		let d, i = Queue.take q in
    visit_order := i::!visit_order;
		dist.(i) <- Some d;
		List.iter (fun x ->
			if parent.(x) = None
			then begin
					parent.(x) <- Some i;
					Queue.add ((d+1), x) q
			end;
		) g.edges.(i);
	done;
	parent.(root) <- None;
	(dist, parent, List.rev !visit_order)

let dfs g root pre post =
	let visites = Array.make (Array.length g.vtx) false in
  let order = ref [] in
	let rec aux i =
    order := i :: !order;
		if visites.(i) then ()
		else begin
			pre i;
			visites.(i) <- true;
			List.iter (aux) g.edges.(i);
			post i
		end
	in aux root;
	visites, !order

exception DependancyCycle
type triEtat =
  | Non_Visite
  | En_cours
  | Visite
let topo_sort g =
  let n = Array.length g.vtx in
  let etats = Array.make n Non_Visite in
  let sortie = ref [] in
  let rec visite i = match etats.(i) with
    | Visite -> ()
    | En_cours -> raise DependancyCycle
    | Non_Visite -> (
      etats.(i) <- En_cours;
      List.iter visite g.edges.(i);
      etats.(i) <- Visite;
      sortie := i :: !sortie
    ) in
  for i=0 to n-1 do
    if etats.(i) <> Visite
    then visite i
  done;
  !sortie