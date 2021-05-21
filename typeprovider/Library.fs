module Library

open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes

[<TypeProvider>]
type MyTypeProvider(cfg : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(cfg)

    let thisAssembly = System.Reflection.Assembly.GetExecutingAssembly()
    let namespaceName = "TypeSafeList"
    let makeOneProvidedType (n : int) =
        let t =
            ProvidedTypeDefinition(
                thisAssembly,
                namespaceName,
                "Type" + string n,
                baseType = Some typeof<obj>)
        t.AddXmlDocDelayed(fun () -> $"""Provided type {"Type" + string n}""")
        let staticProp =
            ProvidedProperty(
                propertyName = "StaticProperty",
                propertyType = typeof<string>,
                isStatic = true,
                getterCode = (fun args -> <@@ "Hello!" @@>))
        staticProp.AddXmlDocDelayed(fun () -> "This is a static property")
        t.AddMember staticProp
        let ctor1 =
            ProvidedConstructor(
                parameters = [],
                invokeCode = (fun args -> <@@ "The object data" :> obj @@>))
        ctor1.AddXmlDocDelayed(fun () -> "This is a constructor")
        t.AddMember ctor1
        let ctor2 =
            ProvidedConstructor(
                parameters = [ ProvidedParameter("data", typeof<string>) ],
                invokeCode = (fun args -> <@@ (%%(args.[0]) : string) :> obj @@>))
        t.AddMember ctor2
        t

    let types = [ for i in 1 .. 100 -> makeOneProvidedType i]    
    do this.AddNamespace(namespaceName, types)

[<assembly:TypeProviderAssembly>]
do ()
