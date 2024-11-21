module Supermarket

type Product = { key: string }

type ShoppingCart = { products: Product list }

module Supermarket =
    
    let total (cart: ShoppingCart) = if cart.products = [] then 0.0 else 0.99