open System

exception SyntaxException

let tokenize s =
    let stream = (new IO.StringReader(s))

    let rec tokenize' (stream: IO.StringReader) =
        match stream.Read() with
        | -1 -> []
        | c -> Convert.ToChar c :: tokenize' stream

    tokenize' stream
    |> List.filter (fun c -> c <> ' ')

let parse tokens =
    let intoInt (c: char) =
        (Convert.ToInt32 c) - Convert.ToInt32 '0'

    let digit c = '0' <= c && c <= '9'

    let peek tokens = List.tryHead tokens

    let eat (tokens: 'a list) = tokens.Tail

    let rec integer i tokens =
        match peek tokens with
        | Some c when digit c -> eat tokens |> integer (10 * i + intoInt c)
        | _ -> (tokens, Convert.ToInt32 i)

    and atom tokens =
        match peek tokens with
        | Some '(' ->
            let (updated, e) = eat tokens |> expression

            match peek updated with
            | Some ')' -> (eat updated, e)
            | _ -> raise (SyntaxException)
        | Some c when digit c -> integer 0 tokens
        | _ -> raise (SyntaxException)

    and expression tokens = term tokens |> expTail

    and expTail (tokens, i) =
        match peek tokens with
        | Some '+' ->
            eat tokens
            |> atom
            |> (fun (t, v) -> (t, (v + i)))
            |> expTail
        | Some '-' ->
            eat tokens
            |> atom
            |> (fun (t, v) -> (t, (v - i)))
            |> expTail
        | _ -> (tokens, i)

    and term tokens = atom tokens |> termTail

    and termTail (tokens, i) =
        match peek tokens with
        | Some '*' ->
            eat tokens
            |> term
            |> (fun (t, v) -> (t, (v * i)))
            |> termTail
        | Some '-' ->
            eat tokens
            |> term
            |> (fun (t, v) -> (t, (v / i)))
            |> termTail
        | _ -> (tokens, i)

    let (_, i) = expression tokens
    i

[<EntryPoint>]
let main argv =
    assert (12 = (tokenize "3 * 4" |> parse))
    assert (21 = (tokenize "(1 + 2) * (3 + 4)" |> parse))
    0 // return an integer exit code
