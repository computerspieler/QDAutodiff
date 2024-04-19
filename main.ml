(* 
Fichier principal du compilateur d'expression
(inspir� de JC Filli�tre
*)

open Format
open Lexing

(* Option de compilation, pour s'arr�ter � l'issue du parser *)
let parse_only = ref false

(* Noms des fichiers source et cible *)
let ifile = ref ""
let ofile = ref ""

let set_file f s = f := s 

(* Les options du compilateur que l'on affiche en donnant l'argument --help *)
let options = 
  ["-parse-only", Arg.Set parse_only, 
   "  Pour ne faire uniquement que la phase d'analyse syntaxique";
   "-o", Arg.String (set_file ofile), 
   "<file>  Pour indiquer le mom du fichier de sortie"]

let usage = "usage: main.exe [option] file.exp"

(* localise une erreur en indiquant la ligne et la colonne *)
let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l (c-1) c

let () = 
  (* Parsing de la ligne de commande *)
  Arg.parse options (set_file ifile) usage;

  (* On v�rifie que le nom du fichier source a bien �t� indiqu� *)
  if !ifile="" then begin eprintf "Aucun fichier � compiler\n@?"; exit 1 end; 

  (* Ce fichier doit avoir l'extension .exp *)
  if not (Filename.check_suffix !ifile ".exp") then begin
    eprintf "Le fichier d'entr�e doit avoir l'extension .exp\n@?";
    Arg.usage options usage;
    exit 1
  end;

  (* Par d�faut, le fichier cible a le m�me nom que le fichier source, 
     seule l'extension change *)
  if !ofile="" then ofile := Filename.chop_suffix !ifile ".exp" ^ ".s";
  
  (* Ouverture du fichier source en lecture *)
  let f = open_in !ifile in
    
  (* Cr�ation d'un tampon d'analyse lexicale *)
  let buf = Lexing.from_channel f in
  
  try
    (* Parsing: la fonction  Parser.prog transforme le tampon lexical en un 
       arbre de syntaxe abstraite si aucune erreur (lexicale ou syntaxique) 
       n'est d�tect�e.
       La fonction Lexer.token est utilis�e par Parser.prog pour obtenir 
       le prochain token. *)
    let p = Parser.prog Lexer.token buf in
    close_in f;
    
    (* On s'arr�te ici si on ne veut faire que le parsing *)
    if !parse_only then exit 0;
    
    (* Compilation de l'arbre de syntaxe abstraite p. Le code machine 
       r�sultant de cette transformation doit �tre �crit dans le fichier 
       cible ofile. *)
    Autodiff.build_program p !ofile
  with
    | Lexer.Lexing_error c -> 
	(* Erreur lexicale. On r�cup�re sa position absolue et 
	   on la convertit en num�ro de ligne *)
	localisation (Lexing.lexeme_start_p buf);
	eprintf "Erreur dans l'analyse lexicale: %c@." c;
	exit 1
    | Parser.Error -> 
	(* Erreur syntaxique. On r�cup�re sa position absolue et on la 
	   convertit en num�ro de ligne *)
	localisation (Lexing.lexeme_start_p buf);
	eprintf "Erreur dans l'analyse syntaxique@.";
	exit 1
  | Ast.ParsingError s -> 
  (* Erreur syntaxique. On r�cup�re sa position absolue et on la 
    convertit en num�ro de ligne *)
  localisation (Lexing.lexeme_start_p buf);
  eprintf "Erreur lors de l'analyse syntaxique: %s@." s;
  exit 1
  | Autodiff.AutodiffError s -> 
  (* Erreur syntaxique. On r�cup�re sa position absolue et on la 
  convertit en num�ro de ligne *)
  eprintf "Erreur lors de l'analyse de l'arbre: %s@." s;
  exit 1

  | _ -> exit 1 (* TODO *)
	




