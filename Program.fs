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
    static member inline reverse (x : Nil) = x

type Cons<'car, 'cdr> =
    | Cons of 'car * 'cdr

    static member inline eval ((Cons (car, cdr)) : Cons< ^Car , ^Cdr >) =
        Cons (
            (^Car : (static member eval : ^Car -> ^ECar) car),
            (^Cdr : (static member eval : ^Cdr -> ^ECdr) cdr))

    static member inline append ((Cons (acar, acdr)) : Cons< ^ACar , ^ACdr >, b : ^B) =
        Cons (acar, (^ACdr : (static member inline append : ^ACdr -> ^B -> ^C) acdr, b))

    static member inline reverse ((Cons (car, cdr)) : Cons< ^Car , ^Cdr >) : _
        when ^Cdr : (static member inline reverse : ^Cdr -> ^RCdr) =
        (^RCdr : (static member inline append : ^RCdr -> Cons< ^Car , Nil> -> _)
            (^Cdr : (static member inline reverse : ^Cdr -> ^RCdr) cdr),
            Cons (car, Nil))

    static member inline last ((Cons (car, _)) : Cons< ^Car , Nil >) = car
    static member inline last ((Cons (_, cdr)) : Cons< _ , ^Cdr >) =
        (^Cdr : (static member inline last : ^Cdr -> _) cdr)

type Append<'a, 'b> = Append of 'a * 'b with
    static member inline eval ((Append (a, b)): Append< ^A , ^B >) : _
        when ^A : (static member eval : ^A -> ^EA) =
        (^EA : (static member append : _ * _ -> _) a, b)

type Reverse<'t> = Reverse of 't with
    static member inline eval ((Reverse list) : Reverse< ^T >) : _
        when ^T : (static member eval : ^T -> ^ET) =
        (^ET : (static member reverse : _ -> _) list)

type Last<'t> = Last of 't with
    static member inline eval ((Last list) : Last< ^T >) : _
        when ^T : (static member eval : ^T -> ^ET) =
        (^ET : (static member last : _ -> _) list)

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
    // let err1 = car Nil  // 型エラー
    // let err2 = cdr Nil  // 型エラー
    let rev =
        Reverse list
        |> eval
    printfn "rev = %A" rev  // => Cons (False, Cons (True, Cons (TInt 10, Nil)))
    let last =
        Last list
        |> eval
    printfn "last = %A" last // False
    0
