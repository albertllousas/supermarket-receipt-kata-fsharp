module Supermarket

type Product = { key: string }

type ShoppingCart = { products: Product list }

let prices = Map.ofList [ ("toothpaste", 0.69); ("toothbrush", 0.99) ]

module Supermarket =
    
    let total (cart: ShoppingCart) = cart.products |> List.sumBy (fun product -> prices.[product.key])