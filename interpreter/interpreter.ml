exception Eval_error

type exp = 
    | True
    | False
    | If of exp * exp * exp
    | Num of int
    | IsZero of exp
    | Plus of exp * exp
    | Mult of exp * exp

let rec step (e : exp) = match e with
    | True -> raise Eval_error
    | False -> raise Eval_error
    | If (True, e1, e2) -> e1
    | If (False, e1, e2) -> e2
    | If (e1, e2, e3) -> 
    begin match e1 with
        | Num(n) -> raise Eval_error
        | _ -> let e' = step e1 in If(e', e2, e3)
    end
    | Num(n) -> raise Eval_error
    | IsZero(e) ->
    begin match e with
        | Num(0) -> True
        | Num(_) -> False
        | _ -> raise Eval_error
    end
    | Plus(e1, e2) -> 
        begin match e1 with
            | Num(n) -> let e' = step e2 in Plus(e1, e')
            | _ -> raise Eval_error
        end
    | Mult(e1, e2) -> 
        begin match e1 with
            | Num(n) -> let e' = step e2 in Mult(e1, e')
            | _ -> raise Eval_error
        end

(*let rec multi_step (e : exp) = match e with
    | True -> raise Eval_error
    | False -> raise Eval_error
    | If (True, e1, e2) -> raise Eval_error
    | If (False, e1, e2) -> raise Eval_error
    | If (e1, e2, e3) -> eval e1, e2, e3
    | Num(n) -> raise Eval_error
    | IsZero(e) -> raise Eval_error
    | Plus(e1, e2) -> 
    | Mult(e1, e2) -> 
*)
