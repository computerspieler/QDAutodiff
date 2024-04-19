exception ParsingError of string

(* Syntaxe abstraite pour notre langage *)
type program = stmt list 

and param_dims =
  | DimInt of int list
  | Unknown

and arg = string * param_dims

and stmt = 
  | SParamDecl of arg list
  | SVarDecl of arg * (expr option) 
  | SReturn of string

and expr = 
  | Int of int
  | Float of float
  | Var of string
  | Binop of binop * expr * expr

and binop = Add | Sub | Mul | Div | MatMul

