module Experiment

// open System
// open System.Reflection
open Microsoft.FSharp.Core.CompilerServices

(*
let inline tmethod n = typeof<Type>.GetMethod(n, BindingFlags.Instance ||| BindingFlags.NonPublic)
let inline invoke (t: Type) (m: MethodInfo) p = m.Invoke(t, p) |> unbox

let IsByRefImpl = tmethod "IsByRefImpl"
let IsArrayImpl = tmethod "IsArrayImpl"
let GetAttributeFlagsImpl = tmethod "GetAttributeFlagsImpl"
let IsPointerImpl = tmethod "IsPointerImpl"
let GetNestedType = tmethod "GetNestedType"
let IsPrimitiveImpl = tmethod "IsPrimitiveImpl"
let GetPropertyImpl = tmethod "GetPropertyImpl"
let IsCOMObjectImpl = tmethod "IsCOMObjectImpl"
let HasElementTypeImpl = tmethod "HasElementTypeImpl"
let GetMethodImpl = tmethod "GetMethodImpl"
let GetConstructorImpl = tmethod "GetConstructorImpl"

type DelegatingType(t: Type) =
    inherit Type()
    let invoke x = invoke t x
    override _.IsByRefImpl() = invoke IsByRefImpl [||]
    override _.IsArrayImpl() = invoke IsArrayImpl [||]
    override _.GetAttributeFlagsImpl() = invoke GetAttributeFlagsImpl [||]
    override _.GetMembers bindingAttr = t.GetMembers bindingAttr
    override _.IsPointerImpl() = invoke IsPointerImpl [||]
    override _.GetNestedType(name, bindingAttr) = invoke GetNestedType [|name;bindingAttr|]
    override _.GetNestedTypes bindingAttr = t.GetNestedTypes bindingAttr
    override _.GetProperties bindingAttr = t.GetProperties bindingAttr
    override _.IsPrimitiveImpl() = invoke IsPrimitiveImpl [||]
    override _.GetPropertyImpl(name, bindingAttr, binder, returnType, types, modifiers) = invoke GetPropertyImpl [|name; bindingAttr; binder; returnType; types; modifiers|]
    override _.GetEvents bindingAttr = t.GetEvents bindingAttr
    override _.GetEvent(name, bindingAttr) = t.GetEvent(name, bindingAttr)
    override _.IsCOMObjectImpl() = invoke IsCOMObjectImpl [||]
    override _.GetInterfaces() = t.GetInterfaces()
    override _.GetElementType() = t.GetElementType()
    override _.GetInterface(name, ignoreCase) = t.GetInterface(name, ignoreCase)
    override _.HasElementTypeImpl() = invoke HasElementTypeImpl [||]
    override _.GetFields bindingAttr = t.GetFields bindingAttr
    override _.GetField(name, bindingAttr) = t.GetField(name, bindingAttr)
    override _.GetMethods bindingAttr = t.GetMethods bindingAttr
    override _.UnderlyingSystemType = t.UnderlyingSystemType
    override _.GetMethodImpl(name, bindingAttr, binder, callConvention, types, modifiers) = invoke GetMethodImpl [|name; bindingAttr; binder; callConvention; types; modifiers|]
    override _.BaseType = t.BaseType
    override _.Name = t.Name
    override _.AssemblyQualifiedName = t.AssemblyQualifiedName
    override _.Namespace = t.Namespace
    override _.FullName = t.FullName
    override _.Assembly = t.Assembly
    override _.Module = t.Module
    override _.GUID = t.GUID
    override _.GetConstructors bindingAttr = t.GetConstructors bindingAttr
    override _.GetConstructorImpl(bindingFlags, binder, callConvention, types, modifiers) = invoke GetConstructorImpl [|bindingFlags; binder; callConvention; types; modifiers|]
    override _.GetCustomAttributes(attributeType, ainherit) = t.GetCustomAttributes(attributeType, ainherit)
    override _.GetCustomAttributes ainherit = t.GetCustomAttributes ainherit
    override _.IsDefined(attributeType, ainherit) = t.IsDefined(attributeType, ainherit)
    override _.InvokeMember(name, invokeAttr, binder, target, args, modifiers, culture, namedParameters) = t.InvokeMember(name, invokeAttr, binder, target, args, modifiers, culture, namedParameters)
    override _.GetCustomAttributesData() = upcast ResizeArray<_>()
*)

[<TypeProvider>]
type SimpleTypeProvider(s : TypeProviderConfig) =
    let event = Event<_, _>()
    // 型プロバイダの構成要素のインスタンス化
    interface ITypeProvider with
        // 静的引数を許容する提供される型に、静的引数を適用する
        // プロバイダは、与えられたマングル名を持つ型を返さなければならない
        member _.ApplyStaticArguments(typeWithoutArguments, typeNameWithArguments, staticArguments) =
            typeof<int>
        // 与えられた、論理的に提供されたアセンブリの実際の内容を取得する
        member _.GetGeneratedAssemblyContents(asm) = [||]
        // 与えられた MethodBase を置き換えるための式木を要求する目的で、コンパイラによって呼び出される
        member _.GetInvokerExpression(methodBase, parameters) = <@@ 12 @@>
        // 含まれる名前空間を取得する
        member _.GetNamespaces() =
            let ty = typeof<int>
            let ns =
                { new IProvidedNamespace with
                    // プロバイダが型を注入する名前空間の名称
                    member _.NamespaceName = "ProvidedNamespace"
                    // この名前空間内のサブの名前空間
                    // 外側の名前空間が解析されるまで名前空間の生成を防ぐためのオプショナルなメンバ
                    member _.GetNestedNamespaces() = [||]
                    // トップレベルの型
                    member _.GetTypes() = [| ty |]
                    // コンパイラは、型プロバイダに型 name を問い合わせるためにこのメソッドを呼び出す
                    // リソルバは、name と呼ばれる型を返すか、型が不明な場合は null を返さなければならない
                    member _.ResolveTypeName(typeName) = if typeName = "Foo" then ty else null }
            [| ns |]
        // 提供される型の静的引数を取得する
        member _.GetStaticParameters(typeWithoutArguments) = [||]
        // CLI メタデータイベントとしてコンパイルされる
        // add_EventName, remove_EventName メソッドを生成する
        [<CLIEvent>]
        member _.Invalidate = event.Publish
        member _.Dispose() = ()

(*
[<assembly: TypeProviderAssembly>]
do ()
*)
