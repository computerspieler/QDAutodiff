
/* Analyseur syntaxique pour Arith */

%{
  open Ast
%}

%token <int> CSTI
%token <float> CSTF
%token <string> IDENT
%token PARAM RETURN 
%token EOF 
%token LP RP LB RB
%token PLUS MINUS TIMES DIV DOT
%token EQ
%token COLON COMMA SEMICOLON

/* D�finitions des priorit�s et associativit�s des tokens */

%left PLUS MINUS 
%left TIMES DIV

/* Point d'entr�e de la grammaire */
%start prog

/* Type des valeurs retourn�es par l'analyseur syntaxique */
%type <Ast.program> prog

%%

prog:
| p = list(stmt) EOF { p }
;

dim_values:
| x = CSTI COMMA n = dim_values { x::n }
| x = CSTI { [x] }
;

arg_decl:
| n = IDENT { (n, Unknown) }
| n = IDENT COLON LP dim = dim_values RP { (n, DimInt dim) }
| n = IDENT COLON dim_len = CSTI { (n, Unknown) }
| n = IDENT COLON dim_len = CSTI LP dim = dim_values RP {
    if List.length dim <> dim_len
    then raise (ParsingError "Incompatible dimensions")
    else (n, DimInt dim)
}
;

args_decl:
| a = arg_decl COMMA n = args_decl  { a::n }
| a = arg_decl SEMICOLON            { [a] }
;

stmt:
| PARAM l = args_decl { SParamDecl l }
| v = arg_decl SEMICOLON { SVarDecl (v, None) }
| v = arg_decl EQ e = expr SEMICOLON { SVarDecl (v, Some e) }
| RETURN s = IDENT SEMICOLON { SReturn s }
;
 
expr:
| LP e = expr RP { e }
| l = expr o = op r = expr { Binop(o, l, r) } 
| v = CSTI { Int v }
| v = CSTF { Float v }
| i = IDENT { Var i }
;

%inline op:
| PLUS  { Add }
| MINUS { Sub }
| TIMES { Mul }
| DIV   { Div }
| DOT   { MatMul }
;

