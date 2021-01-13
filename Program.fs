exception SyntaxException

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

    let peek tokens = List.tryHead tokens

    let eat (tokens: 'a list) = tokens.Tail

    let rec integer tokens i =
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
            | _ -> raise (SyntaxException)
        | Some c when digit c -> integer tokens 0
        | _ -> raise (SyntaxException)

    and expression tokens =
        let (updated, t) = term tokens
        expTail updated t

    and expTail tokens i =
        match peek tokens with
        | Some '+' ->
            let (updated, v) = atom (eat tokens)
            expTail updated (i + v)
        | Some '-' ->
            let (updated, v) = atom (eat tokens)
            expTail updated (i - v)
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
let main argv =
    assert (12 = (tokenize "3 * 4" |> parse))
    assert (21 = (tokenize "(1 + 2) * (3 + 4)" |> parse))
    0 // return an integer exit code
