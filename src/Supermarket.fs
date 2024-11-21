module Supermarket

type Product = { key: string }

type ShoppingCart = { products: Product list }

module Supermarket =
    
    let total (_: ShoppingCart) = 0.0