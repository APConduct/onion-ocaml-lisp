type expr =
  | Atom of string
  | Number of int
  | List of expr list
  | Quote of expr
  | TypedAtom of string * expr
  | CompTime of expr
  | CompTimeAssign of string * expr
  | CompTimeIf of expr * expr * expr
  | QuasiQuote of expr
  | Unquote of expr
  | MacroDef of string * string list * expr
  | MacroCall of string * expr list
