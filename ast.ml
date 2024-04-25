exception ParsingError of string

(* Syntaxe abstraite pour notre langage *)
type 'a program = ('a stmt) list 

and param_dims =
  | DimInt of int list
  | Unknown

and arg = string * param_dims

and 'a stmt = 'a stmt_node * 'a
and 'a stmt_node = 
  | SParamDecl of arg list
  | SVarDecl of arg * (('a expr) option) 
  | SReturn of string

and 'a expr = ('a expr_node) * 'a
and 'a expr_node = 
  | Int of int
  | Float of float
  | Vec of ('a expr) * int
  | Var of string
  | Binop of binop * ('a expr) * ('a expr)

and binop = Add | Sub | Mul | Div

