module Supermarket

open FSharpx

type Quantity = Units of int | Kilograms of float

type CartItem = { productKey: string; quantity: Quantity }

type ShoppingCart = { items: CartItem list }

type UnknownProduct = UnknownProduct of string

type ReceiptLine = { description: string; quantity: Quantity; price: float; amount: float }

type Receipt = { total: float; lines: ReceiptLine list }

type Catalog = { prices: Map<string, float> }

module Catalog =

  let getPrice key (catalog: Catalog) =
    match Map.tryFind key catalog.prices with | Some price -> Ok price | None -> Error(UnknownProduct key)

module Supermarket =

  let private toFloat quantity =
    match quantity with
    | Units count -> float count
    | Kilograms weight -> weight
    
  let private createReceiptItem price cartItem = {
        description = cartItem.productKey
        quantity = cartItem.quantity
        price = price
        amount = (toFloat cartItem.quantity * price) |> (fun total -> (floor (total * 100.0))/100.0)
        }
    
  let receipt (cart: ShoppingCart) (catalog: Catalog): Result<Receipt, UnknownProduct> =
    cart.items
      |> List.map (fun product -> (Catalog.getPrice product.productKey catalog))  
      |> Result.sequence
      |> Result.map (fun prices -> List.zip prices cart.items)
      |> Result.map (List.map ( fun (price, item) -> createReceiptItem price item))
      |> Result.map (fun lines -> { total = List.sumBy _.amount lines; lines = lines })
  
  let total (cart: ShoppingCart) (catalog: Catalog): Result<float, UnknownProduct> =
    receipt cart catalog |> Result.map _.total
  