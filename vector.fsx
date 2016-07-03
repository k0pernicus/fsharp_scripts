exception ElementNotFound of string
exception ArrayIndexOutOfBound of string

type Vector<'T when 'T : equality> =
    { mutable elements: 'T array; length: int } 
    static member Init (n: int, e: 'T) : Vector<'T> = { elements = Array.create n e; length = n }
    static member Zero (n: int) : Vector<'T> = { elements = Array.zeroCreate<'T> n; length = n }
    static member Empty : Vector<'T> = { elements = Array.empty<'T>; length = 0 }
    member v.Length = v.elements.Length
    member v.IsEmpty : bool = v.Length = 0
    member v.Resize (n : int) = 
        if n < v.Length then
            v.elements <- Array.sub v.elements 0 n
        else
            v.elements <- Array.append v.elements (Array.zeroCreate<'T> (n - v.Length))
    member v.Resize (b : int, n: int) =
        if b < 0 || b >= v.Length || n <= 0 || n >= v.Length then raise (ArrayIndexOutOfBound("Vector.Resize"))
        else v.elements <- Array.sub v.elements b n
    member v.Add (elt : 'T) = v.Resize (v.Length + 1); v.elements.[v.Length] <- elt
    member v.Remove (elt : 'T) = v.elements <- (v.elements |> Array.filter ((<>) elt))
    member v.Get (i : int) : 'T =
        if i < 0 || i >= v.Length then raise (ArrayIndexOutOfBound("Vector.Get"))
        else v.elements.[i]
    member v.Set (i: int, e: 'T) =
        if i < 0 || i >= v.Length then raise (ArrayIndexOutOfBound("Vector.Set"))
        else v.elements.[i] <- e
    member v.First =
        match v.Length with
            | 0 -> raise (ElementNotFound("Empty list"))
            | _ -> v.elements.[0]
    member v.Pop : 'T=
        match v.Length with
            | 0 -> raise (ElementNotFound("Empty list"))
            | _ -> 
                   let e = v.elements.[v.Length - 1]
                   v.Resize (v.Length - 1)
                   e
    member v.Last =
        match v.Length with
            | 0 -> raise (ElementNotFound("Empty list"))
            | _ -> v.elements.[v.Length - 1]
    member v.Exists (elt : 'T) : bool =
        match (v.elements |> Array.filter ((=) elt)).Length with
            | 0 -> false
            | _ -> true
    member v.Reverse = v.elements |> Array.rev
