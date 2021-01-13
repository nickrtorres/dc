exception Syntax

let tokenize (s: string) =
    let stream = (new System.IO.StringReader(s))

    let rec tokenize' (stream: System.IO.StringReader) =
        match stream.Read() with
        | -1 -> []
        | c -> System.Convert.ToChar c :: tokenize' stream

    List.filter (fun c -> c <> ' ') (tokenize' stream)

let parse tokens =
    let intoInt (c: char) =
        (System.Convert.ToInt32 c)
        - System.Convert.ToInt32 '0'

    let digit c = '0' <= c && c <= '9'

    let peek tokens =
        match tokens with
        | [] -> None
        | hd :: tl -> Some(hd)

    let eat tokens =
        match tokens with
        | [] -> []
        | hd :: tl -> tl

    let rec integer tokens i =
        printfn "i => %A" i

        match peek tokens with
        | Some c when digit c -> integer (eat tokens) (10 * i + intoInt c)
        | Some _
        | None -> (tokens, System.Convert.ToInt32 i)

    and atom tokens =
        match peek tokens with
        | Some '(' ->
            let (updated, e) = eat tokens |> expression

            match peek updated with
            | Some ')' -> (eat updated, e)
            | _ -> raise (Syntax)
        | Some c when digit c -> integer tokens 0
        | _ -> raise (Syntax)

    and expression tokens =
        let (updated, t) = term tokens
        expTail updated t

    and expTail (tokens: char list, i: int) =
        match peek tokens with
        | Some '+' -> eat tokens >> atom >> expTail
        | Some '-' -> eat tokens |> atom |> expTail
        | _ -> (tokens, i)

    and term tokens =
        let (updated, a) = atom tokens
        termTail updated a

    and termTail tokens i =
        match peek tokens with
        | Some '*' ->
            let (updated, t) = term (eat tokens)
            termTail updated (i * t)
        | Some '-' ->
            let (updated, t) = term (eat tokens)
            termTail updated (i / t)
        | _ -> (tokens, i)

    let (_, i) = expression tokens
    i

[<EntryPoint>]
let main argv = 0 // return an integer exit code