module Supermarket

open FSharpx

module Types =

  type Quantity = Units of int | Kilograms of float

  type CartItem = { productKey: string; quantity: Quantity }

  type ShoppingCart = { items: CartItem list }
  
  type Discount = BuyOneGetOneFree

  type DiscountLine = { product: string; discount: Discount; amount: float }
  
  type ReceiptLine = { description: string; quantity: Quantity; price: float; amount: float }

  type Receipt = { total: float; lines: ReceiptLine list; discounts: DiscountLine list }
   
  type UnknownProduct = UnknownProduct of string

  type GetPrice = string -> Result<float, UnknownProduct>
  
  type FindDiscount = string -> Discount option
  
module Defaults =
  open Types
  
  let toFloat quantity = match quantity with Units count -> float count | Kilograms weight -> weight
    
  let getPriceFromMap (catalog: Map<string, float>) : GetPrice = fun key ->
      match Map.tryFind key catalog with | Some price -> Ok price | None -> Error(UnknownProduct key)
      
  let findDiscountFromMap (discounts: Map<string, Discount>) : FindDiscount = fun key -> Map.tryFind key discounts

module Discounts =
  open Types
  open Defaults
  
  let private applyBuyOneGetOneFree receiptLine =
    let freeItems = int (toFloat receiptLine.quantity) / 2 
    let paidItems = int (toFloat receiptLine.quantity) - freeItems
    (float paidItems) * receiptLine.price 
  
  let private createDiscountLine line discount amount = { product = line.description; discount = discount; amount = amount }
  
  let checkDiscount discount line =
    let units = match line.quantity with Units x -> x | Kilograms x -> int x
    match discount with
      | BuyOneGetOneFree -> if units > 1 then Some (createDiscountLine line discount (applyBuyOneGetOneFree line)) else None
      
  let apply receipt findDiscount =
    receipt.lines
      |> List.map (fun line -> (findDiscount line.description) |> Option.map (fun discount -> (discount, line)))
      |> List.choose id
      |> List.map (fun (discount, line) -> checkDiscount discount line)
      |> List.choose id
      |> (fun discounts -> { receipt with discounts = discounts; total = receipt.total - List.sum (discounts |> List.map _.amount) } )
    
module Receipt =
  open Types
  open Defaults
    
  let private createReceiptLine price cartItem = {
    description = cartItem.productKey
    quantity = cartItem.quantity
    price = price
    amount = (toFloat cartItem.quantity * price) |> (fun total -> (floor (total * 100.0))/100.0)
    }  
      
  let provide (cart: ShoppingCart) (findPrice: GetPrice) (findDiscount: FindDiscount): Result<Receipt, UnknownProduct> =
    cart.items
      |> List.map (fun item -> (findPrice item.productKey))  
      |> Result.sequence
      |> Result.map (fun prices -> List.zip prices cart.items)
      |> Result.map (List.map ( fun (price, item) -> createReceiptLine price item))
      |> Result.map (fun lines  -> { lines = lines; total = List.sum (lines |> List.map _.amount); discounts = [] })
      |> Result.map (fun receipt -> Discounts.apply receipt findDiscount)
  
  let total (cart: ShoppingCart) (findPrice: GetPrice) (findDiscount: FindDiscount): Result<float, UnknownProduct> =
    provide cart findPrice findDiscount |> Result.map _.total
  