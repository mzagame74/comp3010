exception Eval_error

type typ = 
    | TBool
    | TInt
    | TArrow of typ * typ

type exp = 
    | True
    | False
    | If of exp * exp * exp
    | Num of int
    | IsZero of exp
    | Plus of exp * exp
    | Mult of exp * exp
    | Var of string
    | Lambda of string * exp
    | Apply of exp * exp
    | Let of string * exp * exp
    | TypeError

type environment = (string * exp) list

let rec string_of_typ (t : typ) = match t with
    | TBool -> "bool"
    | TInt -> "int"
    | TArrow(t1, t2) -> "(" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ ")"

let rec string_of_exp (e : exp) = match e with
    | True -> "true"
    | False -> "false"
    | If(e1, e2, e3) -> "if " ^ string_of_exp e1 ^ " then " ^ string_of_exp e2
        ^ " else " ^ string_of_exp e3
    | Num(n) -> string_of_int n
    | IsZero(e') -> "(isZero " ^ string_of_exp e' ^ ")"
    | Plus(e1, e2) -> "(" ^ string_of_exp e1 ^ " + " ^ string_of_exp e2 ^ ")"
    | Mult(e1, e2) -> "(" ^ string_of_exp e1 ^ " * " ^ string_of_exp e2 ^ ")"
    | Var(str) -> str
    | Lambda(str, e') -> "(lambda " ^ str ^ ":"  ^ "." ^ string_of_exp e' ^ ")"
    | Apply(e1, e2) -> "(" ^ string_of_exp e1 ^ " " ^ string_of_exp e2 ^ ")"
    | Let(str, e1, e2) -> "let " ^ str ^ " = " ^ string_of_exp e1 ^ " in " ^
        string_of_exp e2
    | TypeError -> "typeError"

let rec step (env : environment) (e : exp) : (environment * exp) = match e with
    | If(True, e2, e3) -> env, e2
    | If(False, e2, e3) -> env, e3
    | If(Num(n), e2, e3) -> env, TypeError
    | If(TypeError, e2, e3) -> env, TypeError
    | If(e1, e2, e3) -> let env' = step env e1 in
        env' |> fst, If(env' |> snd, e2, e3)
    | IsZero(Num(0)) -> env, True
    | IsZero(Num(n)) -> env, False
    | IsZero(True) | IsZero(False) -> env, TypeError
    | IsZero(TypeError) -> env, TypeError
    | IsZero(e') -> let env' = step env e' in env' |> fst, IsZero(env' |> snd)
    | Plus(Num(n1), Num(n2)) -> env, Num(n1 + n2)
    | Plus(True, e2) | Plus(False, e2) -> env, TypeError
    | Plus(Num(n), True) | Plus(Num(n), False) -> env, TypeError
    | Plus(TypeError, _) | Plus(_, TypeError) -> env, TypeError
    | Plus(Num(n), e2) -> let env' = step env e2 in
        env' |> fst, Plus(Num(n), env' |> snd)
    | Plus(e1, e2) -> let env' = step env e1 in
        env' |> fst, Plus(env' |> snd, e2)
    | Mult(Num(n1), Num(n2)) -> env, Num(n1 * n2)
    | Mult(True, e2) | Mult(False, e2) -> env, TypeError
    | Mult(Num(n), True) | Mult(Num(n), False) -> env, TypeError
    | Mult(TypeError, _) | Mult(_, TypeError) -> env, TypeError
    | Mult(Num(n), e2) -> let env' = step env e2 in
        env' |> fst, Mult(Num(n), env' |> snd)
    | Mult(e1, e2) -> let env' = step env e1 in
        env' |> fst, Mult(env' |> snd, e2)
    | Var(str) -> env, (try List.assoc str env with
        | Not_found -> TypeError)
    | Apply(Lambda(str, e'), True) -> ((str, True)::env), e'
    | Apply(Lambda(str, e'), False) -> ((str, False)::env), e'
    | Apply(Lambda(str, e'), Num(n)) -> ((str, Num(n))::env), e'
    | Apply(Lambda(str1, e1), Lambda(str2, e2)) ->
        (str1, Lambda(str2, e2))::env, e1
    | Apply(Lambda(str, e'), TypeError) -> env, TypeError
    | Apply(Lambda(str, e'), e2) ->
        env, Apply(Lambda(str, e'), step env e2 |> snd)
    | Apply(True, e2) | Apply(False, e2) -> env, TypeError
    | Apply(Num(n), e2) -> env, TypeError
    | Apply(TypeError, e2) -> env, TypeError
    | Apply(e1, e2) -> env, Apply(step env e1 |> snd, e2)
    | Let(str, e1, e2) -> ((str, e1)::env), (Apply(Lambda(str, e2), e1))
    | _ -> raise Eval_error

let rec multi_step (env : environment) (e : exp) : (environment * exp) =
    match e with
    | True -> env, True
    | False -> env, False
    | Num(n) -> env, Num(n)
    | Lambda(str, e') -> env, Lambda(str, e')
    | TypeError -> env, TypeError
    | e' -> let env' = step env e' in multi_step (env' |> fst) (env' |> snd)
