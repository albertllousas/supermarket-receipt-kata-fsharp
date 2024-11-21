module Supermarket

open FSharpx

type Product = { key: string }

type UnknownProduct = UnknownProduct of string

type ShoppingCart = { products: Product list }

let prices = Map.ofList [ ("toothpaste", 0.69); ("toothbrush", 0.99) ]

module Supermarket =

  let total (cart: ShoppingCart) : Result<float, UnknownProduct> =
    let getPrice key = match Map.tryFind key prices with | Some price -> Ok price | None -> Error (UnknownProduct key)
    cart.products
    |> List.map (fun product -> getPrice product.key)
    |> Result.sequence
    |> Result.map (fun prices -> List.sum prices)
