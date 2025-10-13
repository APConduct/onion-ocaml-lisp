type instruction =
  | PUSH_NUM of int
  | PUSH_ATOM of string
  | ADD
  | SUB
  | MUL
  | EQ
  | LT
  | GT
  | LIST of int
  | CAR
  | CDR
  | CALL_BUILTIN of string
  | IF of instruction list * instruction list
  | QUOTE of value
  | POP

and value =
  | VNum of int
  | VAtom of string
  | VList of value list
