type typ =
  | TInt
  | TAtom
  | TList of typ
  | TFn of typ list * typ
  | TVar of string
  | TAny

type type_env = (string * typ)
list
