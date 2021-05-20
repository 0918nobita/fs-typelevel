let inline eval (x : ^X) =
    (^X : (static member eval : ^X -> ^EX) x)

type True = True with
    static member inline eval (x : True) = x

type False = False with
    static member inline eval (x : False) = x

type TInt = TInt of int with
    static member inline eval (x : TInt) = x

type Nil = Nil with
    static member inline eval (x : Nil) = x
    static member inline append (_ : Nil, b : ^B) = b

type Cons<'car, 'cdr> =
    | Cons of 'car * 'cdr

    static member inline eval ((Cons (car, cdr)) : Cons< ^Car , ^Cdr >) =
        Cons (
            (^Car : (static member eval : ^Car -> ^ECar) car),
            (^Cdr : (static member eval : ^Cdr -> ^ECdr) cdr))

    static member inline append ((Cons (acar, acdr)) : Cons< ^ACar , ^ACdr >, b : ^B) =
        Cons (acar, (^ACdr : (static member inline append : ^ACdr -> ^B -> ^C) acdr, b))

type Append<'a, 'b> = Append of 'a * 'b with
    static member inline eval ((Append (a, b)): Append< ^A , ^B >) : _
        when ^A : (static member eval : ^A -> ^EA) =
        (^EA : (static member append : _ * _ -> _) a, b)

let inline car ((Cons (car', _)) : Cons< ^A , ^B >) = car'

let inline cdr ((Cons (_, cdr')) : Cons< ^A , ^B >) = cdr'

[<EntryPoint>]
let main _ =
    let listA = Cons (TInt 10, Cons (True, Nil))
    let listB = Cons (False, Nil)
    let list =
        Append (listA, listB)
        |> eval
    printfn "list = %A" list  // => Cons (TInt 10, Cons (True, Cons (False, Nil)))
    let car' = car list
    printfn "car' = %A" car'  // => TInt 10
    let cdr' = cdr list
    printfn "cdr' = %A" cdr'  // => Cons (True, Cons (False, Nil))
    0
