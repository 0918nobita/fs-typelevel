module Experiment

open Microsoft.FSharp.Core.CompilerServices

(*
[<assembly: TypeProviderAssembly>]
do ()
*)

type ProvidedNamespace () =
    // 型プロバイダの構成要素から提供される名前空間
    interface IProvidedNamespace with
        // プロバイダが型を注入する名前空間の名称
        member val NamespaceName = "ProvidedNamespace" with get
        // この名前空間内のサブの名前空間
        // 外側の名前空間が解析されるまで名前空間の生成を防ぐためのオプショナルなメンバ
        member _.GetNestedNamespaces() = [||]
        // トップレベルの型
        member _.GetTypes() = [||]
        // コンパイラは、型プロバイダに型 name を問い合わせるためにこのメソッドを呼び出す
        // リソルバは、name と呼ばれる型を返すか、型が不明な場合は null を返さなければならない
        member _.ResolveTypeName(typeName) = null

(*
[<TypeProvider>]
type SimpleTypeProvider(s : TypeProviderConfig) =
    interface ITypeProvider with
        member _.ApplyStaticArguments(typeWithoutArguments, typeNameWithArguments, staticArguments) =
            typeof<int>
        member _.GetGeneratedAssemblyContents(asm) = [||]
        member _.GetInvokerExpression(methodBase, parameters) = <@@ 12 @@>
        member _.GetStaticParameters(typeWithoutArguments) = [||]
        // 含まれる名前空間を取得する
        member _.GetNamespaces() = [| ProvidedNamespace() |]
*)
