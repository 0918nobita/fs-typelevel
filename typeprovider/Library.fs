module Library

open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes

[<TypeProvider>]
type MyTypeProvider(cfg : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(cfg)

    let thisAssembly = System.Reflection.Assembly.GetExecutingAssembly()
    let namespaceName = "MyTypeProvider"
    let ty = ProvidedTypeDefinition(thisAssembly, namespaceName, "WithExclamationMark", Some typeof<obj>)
    let staticParams = [ProvidedStaticParameter("literal", typeof<string>)]
    do ty.DefineStaticParameters(
        parameters = staticParams,
        instantiationFunction = (fun typeName paramValues ->
            match paramValues with
            | [| :? string as pattern |] ->
                let ty = ProvidedTypeDefinition(thisAssembly, namespaceName, typeName, Some typeof<obj>)
                let staticProp =
                    ProvidedProperty(
                        propertyName = "Value",
                        propertyType = typeof<string>,
                        isStatic = true,
                        getterCode = (fun _ -> <@@ pattern + "!" @@>))
                staticProp.AddXmlDocDelayed (fun () -> $"string `{pattern}!`")
                ty.AddMember staticProp
                ty
            | _ -> failwith "unexpected parameter values"))
    do this.AddNamespace(namespaceName, [ty])

[<assembly:TypeProviderAssembly>]
do ()
