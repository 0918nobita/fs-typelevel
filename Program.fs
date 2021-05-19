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

type Nil = Nil

type Cons< ^Car, ^Cdr > =
    | Cons of ^Car * ^Cdr

    static member inline car (list : Cons< ^Car, ^Cdr >) : ^Car =
        match list with
        | Cons (car', _) -> car'

    static member inline cdr (list : Cons< ^Car , ^Cdr >) : ^Cdr =
        match list with
        | Cons (_, cdr') -> cdr'

    // static member inline append (listA : Cons< ^ACar , ^ACdr >) (listB : Cons< ^BCar , ^BCdr >) =

(*
type Append<'a, 'b> = Append of 'a * 'b with
    static member inline eval (_ : Ty<Append< ^A , Nil>>) = ty< ^A >
    static member inline eval (_ : Ty<Append< Nil, ^B >>) = ty< ^B >
    static member inline eval (_ : Ty<Append< ^A , ^B >>) : _
        when ^A : (static member car : Ty< ^A > -> Ty< ^ACar >)
        and ^A : (static member cdr : Ty< ^A > -> Ty< ^ACdr >) = ty<Cons< ^ACar , Append< ^ACdr , ^B >>>
*)

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

    (*
    // FIXME: 1 回の eval で Append<_,_> 全部展開してほしい～～～
    let list =
        ty<Append<Cons<True, Cons<False, Nil>>, Cons<True, Nil>>>
        |> eval
        |> eval
        |> eval
    printfn "%s: %A" (nameof list) list
    *)

    let list = Cons (1, Cons(2, Nil))
    let list2 = Cons.cdr list
    let list3 = Cons.cdr list2
    // let list4 = Cons.cdr list3 // 型エラー
    0
