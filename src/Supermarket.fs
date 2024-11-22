module Supermarket

open FSharpx

type CartItem = { productKey: string; quantity: float }

type ShoppingCart = { items: CartItem list }

type UnknownProduct = UnknownProduct of string

type Catalog = { prices: Map<string, float> }

module Catalog =

  let getPrice key (catalog: Catalog) =
    match Map.tryFind key catalog.prices with | Some price -> Ok price | None -> Error(UnknownProduct key)

module Supermarket =

  let total (cart: ShoppingCart) (catalog: Catalog): Result<float, UnknownProduct> =
    cart.items
    |> List.map (fun product -> (Catalog.getPrice product.productKey catalog))  
    |> Result.sequence
    |> Result.map (fun prices -> List.zip prices cart.items)
    |> Result.map (List.sumBy (fun (price, item) -> price * item.quantity))
    |> Result.map (fun total -> (floor (total * 100.0))/100.0)
