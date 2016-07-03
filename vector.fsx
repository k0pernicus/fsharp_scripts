exception ElementNotFound of string
exception ArrayIndexOutOfBound of string

type Vector<'T when 'T : equality> =
    { mutable elements: 'T array; mutable length: int } 
    static member Init (n: int, e: 'T) : Vector<'T> = { elements = Array.create n e; length = n }
    static member Zero (n: int) : Vector<'T> = { elements = Array.zeroCreate<'T> n; length = n }
    static member Empty : Vector<'T> = { elements = Array.empty<'T>; length = 0 }
    member v.Length = v.length
    member v.IsEmpty : bool = v.length = 0
    member v.Resize (n : int) = 
        if n < v.length then v.elements <- Array.sub v.elements 0 n
        else v.elements <- Array.append v.elements (Array.zeroCreate<'T> (n - v.length))
        v.length <- n
    member v.Clear =
        v.elements <- Array.zeroCreate<'T> v.length
    member v.Resize (b : int, n: int) =
        if b < 0 || b >= v.length || n <= 0 || n >= v.length then raise (ArrayIndexOutOfBound("Vector.Resize"))
        else v.elements <- Array.sub v.elements b n
        v.length <- n
    member v.Add (elt : 'T) = v.Resize (v.length + 1); v.elements.[v.length] <- elt; v.length <- (v.length + 1)
    member v.Remove (elt : 'T) = v.elements <- (v.elements |> Array.filter ((<>) elt)); v.length <- v.elements.Length
    member v.Get (i : int) : 'T =
        if i < 0 || i >= v.length then raise (ArrayIndexOutOfBound("Vector.Get"))
        else v.elements.[i]
    member v.Set (i: int, e: 'T) =
        if i < 0 || i >= v.length then raise (ArrayIndexOutOfBound("Vector.Set"))
        else v.elements.[i] <- e
    member v.First =
        match v.length with
            | 0 -> raise (ElementNotFound("Empty list"))
            | _ -> v.elements.[0]
    member v.Pop : 'T=
        match v.length with
            | 0 -> raise (ElementNotFound("Empty list"))
            | _ -> 
                   let e = v.elements.[v.Length - 1]
                   v.Resize (v.Length - 1)
                   v.length <- v.Length - 1
                   e
    member v.Last =
        match v.length with
            | 0 -> raise (ElementNotFound("Empty list"))
            | _ -> v.elements.[v.Length - 1]
    member v.Exists (elt : 'T) : bool =
        match (v.elements |> Array.filter ((=) elt)).Length with
            | 0 -> false
            | _ -> true
    member v.Reverse = v.elements |> Array.rev
