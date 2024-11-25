module Supermarket

open FSharpx

module Types =

  type Quantity = Units of int | Kilograms of float

  type CartItem = { productKey: string; quantity: Quantity }

  type ShoppingCart = { items: CartItem list }

  type ReceiptLine = { description: string; quantity: Quantity; price: float; amount: float }

  type Receipt = { total: float; lines: ReceiptLine list }
   
  type UnknownProduct = UnknownProduct of string

  type GetPrice = string -> Result<float, UnknownProduct>
  
module Defaults =
  open Types
    
  let getPriceFromMap (catalog: Map<string, float>) : GetPrice = fun key ->
      match Map.tryFind key catalog with | Some price -> Ok price | None -> Error(UnknownProduct key)

module Receipt =
  open Types

  let private toFloat quantity =
    match quantity with Units count -> float count | Kilograms weight -> weight
    
  let private createReceiptLine price cartItem = {
    description = cartItem.productKey
    quantity = cartItem.quantity
    price = price
    amount = (toFloat cartItem.quantity * price) |> (fun total -> (floor (total * 100.0))/100.0)
    }  
      
  let provide (cart: ShoppingCart) (findPrice: GetPrice): Result<Receipt, UnknownProduct> =
    cart.items
      |> List.map (fun item -> (findPrice item.productKey))  
      |> Result.sequence
      |> Result.map (fun prices -> List.zip prices cart.items)
      |> Result.map (List.map ( fun (price, item) -> createReceiptLine price item))
      |> Result.map (fun lines  -> { lines = lines; total = List.sum (lines |> List.map _.amount) })
  
  let total (cart: ShoppingCart) (findPrice: GetPrice): Result<float, UnknownProduct> =
    provide cart findPrice |> Result.map _.total
  