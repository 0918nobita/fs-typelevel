[<Struct>]
type Ty<'Type> =
    override _.ToString() =
        typeof<'Type>
            .ToString()
            .Replace("[", "<")
            .Replace("]", ">")
            .Replace("`1", "")
            .Replace("`2", "")

let inline ty< ^T > : Ty< ^T > = Ty()

type True = True with
    static member inline eval (_ : Ty<True>) = ty<True>
    static member inline ifThenElse (_ : Ty<True>, x, _) = x

type False = False with
    static member inline eval (_ : Ty<False>) = ty<False>
    static member inline ifThenElse (_ : Ty<False>, _, y) = y

let inline eval (x : Ty< ^A >) : Ty< ^B > =
    (^A : (static member inline eval : Ty< ^A > -> Ty< ^B >) x)

type Not<'a> = Not of 'a with
    static member inline eval (_ : Ty<Not< ^A >>) : Ty< ^B > when ^A : (static member eval : Ty< ^A > -> Ty< ^Bool >) =
        (^Bool : (static member ifThenElse : Ty< ^Bool > * _ * _ -> Ty< ^B >) ty< ^Bool >, ty<False>, ty<True>)

type BadType = BadType with
    static member inline eval (_ : Ty<BadType>) = ty<BadType>

type Nil = Nil with
    static member inline eval (_ : Ty<Nil>) = ty<Nil>

type Cons<'car, 'cdr> = Cons of 'car * 'cdr with
    static member inline eval (_ : Ty<Cons< ^Car , ^Cdr >>) : _
        when ^Car : (static member eval : Ty< ^Car > -> Ty< ^ECar >)
        and ^Cdr : (static member eval : Ty< ^Cdr > -> Ty< ^ECdr >) = ty<Cons< ^ECar , ^ECdr >>
    static member inline car (_ : Ty<Cons< ^Car, _ >>) = ty< ^Car >
    static member inline cdr (_ : Ty<Cons< _, ^Cdr >>) = ty< ^Cdr >

type Append<'a, 'b> = Append of 'a * 'b with
    static member inline eval (_ : Ty<Append< ^A , Nil>>) = ty< ^A >
    static member inline eval (_ : Ty<Append< Nil, ^B >>) = ty< ^B >
    static member inline eval (_ : Ty<Append< ^A , ^B >>) : _
        when ^A : (static member car : Ty< ^A > -> Ty< ^ACar >)
        and ^A : (static member cdr : Ty< ^A > -> Ty< ^ACdr >) = ty<Cons< ^ACar , Append< ^ACdr , ^B >>>

[<EntryPoint>]
let main _ =
    let true_ = eval ty<True> // : Ty<True>
    printfn "%s: %A" (nameof true_) true_
    let false_ = eval ty<False> // : Ty<False>
    printfn "%s: %A" (nameof false_) false_
    let notTrue = eval ty<Not<True>> // : Ty<False>
    printfn "%s: %A" (nameof notTrue) notTrue
    let notFalse = eval ty<Not<False>> // : Ty<True>
    printfn "%s: %A" (nameof notFalse) notFalse
    let notNotTrue = eval ty<Not<Not<True>>> // : Ty<True>
    printfn "%s: %A" (nameof notNotTrue) notNotTrue
    // let err1 = eval ty<int> // 型 int は演算子 eval をサポートしていません
    // let err2 = eval ty<Not<BadType>> // 型 BadType は演算子 ifThenElse をサポートしていません

    // FIXME: 1 回の eval で Append<_,_> 全部展開してほしい～～～
    let list =
        ty<Append<Cons<True, Cons<False, Nil>>, Cons<True, Nil>>>
        |> eval
        |> eval
        |> eval
    printfn "%s: %A" (nameof list) list
    0
