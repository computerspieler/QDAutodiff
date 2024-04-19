
(* Analyseur lexical *)

{
  open Lexing
  open Parser
   
  exception Lexing_error of char
    
  let kwd_tbl = [
      "param", PARAM;
      "return", RETURN
  ]
  let id_or_kwd s = try List.assoc s kwd_tbl with _ -> IDENT s

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

}

let letter = ['a'-'z' 'A'-'Z' '_']
let digit = ['0'-'9']
let ident = letter (letter | digit)*
let integer = ['0'-'9']+
let float = integer '.' digit*
let space = [' ' '\t']

rule token = parse
  | "/*" ([^'*']|'*'[^'/'])* "*/" as c	
	{
		String.iter (fun c -> if c = '\n' then newline lexbuf;) c;
		token lexbuf
	}
  | '\n'    { newline lexbuf; token lexbuf }
  | space+  { token lexbuf }
  | ident as id { id_or_kwd id }
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '*'     { TIMES }
  | '/'     { DIV }
  | '.'     { DOT }
  | '='     { EQ }
  | '('     { LP }
  | ')'     { RP }
  | '{'     { LB }
  | '}'     { RB }
  | ':'     { COLON }
  | ','     { COMMA }
  | ';'     { SEMICOLON }
  | integer as s { CSTI (int_of_string s) }
  | float as s { CSTF (float_of_string s) }
  | eof     { EOF }
  | _ as c  { raise (Lexing_error c) }
 

