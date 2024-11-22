module Supermarket

open FSharpx

type Product = { key: string }

type UnknownProduct = UnknownProduct of string

type ShoppingCart = { products: Product list }

type Catalog = { prices: Map<string, float> }

module Catalog =

  let getPrice key (catalog: Catalog) =
    match Map.tryFind key catalog.prices with | Some price -> Ok price | None -> Error(UnknownProduct key)

module Supermarket =

  let total (cart: ShoppingCart) (catalog: Catalog): Result<float, UnknownProduct> =
    cart.products
    |> List.map (fun product -> Catalog.getPrice product.key catalog)  
    |> Result.sequence
    |> Result.map (fun prices -> List.sum prices)
