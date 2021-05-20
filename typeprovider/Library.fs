module Library

open FSharp.Core.CompilerServices
// open ProviderImplementation
open ProviderImplementation.ProvidedTypes

[<TypeProvider>]
type MyTypeProvider(cfg : TypeProviderConfig) =
    inherit TypeProviderForNamespaces(cfg)

    let asm = System.Reflection.Assembly.GetExecutingAssembly()
    let ns = "TypeSafeList"
    // なにがしたいのか、よくわからん
    let listProvTy =
        ProvidedTypeDefinition(
            assembly = asm,
            namespaceName = ns,
            className = "TypeSafeList",
            baseType = None,
            hideObjectMethods = true,
            nonNullable = true)
