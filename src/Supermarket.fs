module Supermarket

open FSharpx

type Quantity = Units of int | Kilograms of float

type CartItem = { productKey: string; quantity: Quantity }

type ShoppingCart = { items: CartItem list }

type UnknownProduct = UnknownProduct of string

type Catalog = { prices: Map<string, float> }

module Catalog =

  let getPrice key (catalog: Catalog) =
    match Map.tryFind key catalog.prices with | Some price -> Ok price | None -> Error(UnknownProduct key)

module Supermarket =
  
  let toFloat quantity =
    match quantity with
    | Units count -> float count
    | Kilograms weight -> weight

  let total (cart: ShoppingCart) (catalog: Catalog): Result<float, UnknownProduct> =
    cart.items
    |> List.map (fun product -> (Catalog.getPrice product.productKey catalog))  
    |> Result.sequence
    |> Result.map (fun prices -> List.zip prices cart.items)
    |> Result.map (List.sumBy (fun (price, item) -> (toFloat item.quantity * price)))
    |> Result.map (fun total -> (floor (total * 100.0))/100.0)
