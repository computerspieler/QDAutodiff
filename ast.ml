(* Syntaxe abstraite pour notre langage *)

type program = stmt list 

and stmt = 
  | Read of string
  | Print of expr
  | Function of string*string*expr

and expr = 
  | Cst of int
  | Var of string
  | Binop of binop * expr * expr
  | Letin of string * expr * expr
  | Call of string * expr
  | Arg

and binop = Add | Sub | Mul | Div

