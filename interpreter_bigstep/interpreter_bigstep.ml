exception Eval_error

type exp = 
    | True
    | False
    | If of exp * exp * exp
    | Num of int
    | IsZero of exp
    | Plus of exp * exp
    | Mult of exp * exp

let rec string_of_exp (e : exp) = match e with
    | True -> "true"
    | False -> "false"
    | If(e1, e2, e3) -> "if " ^ string_of_exp e1 ^ " then " ^ string_of_exp e2
        ^ " else " ^ string_of_exp e3
    | Num(n) -> string_of_int n
    | IsZero(e1) -> "(isZero " ^ string_of_exp e1 ^ ")"
    | Plus(e1, e2) -> "(" ^ string_of_exp e1 ^ " + " ^ string_of_exp e2 ^ ")"
    | Mult(e1, e2) -> "(" ^ string_of_exp e1 ^ " * " ^ string_of_exp e2 ^ ")"

let rec eval (e : exp) = match e with
    | True -> True
    | False -> False
    | If(e1, e2, e3) -> (match eval e1 with
        | True -> eval e2
        | False -> eval e3
        | _ -> raise Eval_error)
    | Num(n) -> Num(n)
    | IsZero(e) -> (match eval e with
        | Num(0) -> True
        | Num(n) when n != 0 -> False
        | _ -> raise Eval_error)
    | Plus(e1, e2) -> (match eval e1, eval e2 with
        | Num(n1), Num(n2) -> Num(n1 + n2)
        | Num(n1), _ -> raise Eval_error
        | _, Num(n2) -> raise Eval_error
        | _, _ -> raise Eval_error)
    | Mult(e1, e2) -> (match eval e1, eval e2 with
        | Num(n1), Num(n2) -> Num(n1 * n2)
        | Num(n1), _ -> raise Eval_error
        | _, Num(n2) -> raise Eval_error
        | _, _ -> raise Eval_error)
