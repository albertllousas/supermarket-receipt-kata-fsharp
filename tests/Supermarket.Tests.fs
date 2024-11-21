module SupermarketTests

open Expecto
open Supermarket

let assertThat actual expected = Expect.equal expected actual ""

[<Tests>]
let tests = testList "Supermarket tests" [

  test "Should calculate the total of an empty shopping cart" {
    let shoppingCart = { products = []}
    assertThat (Supermarket.total shoppingCart) (Ok 0.0)  
  }
  
  test "Should calculate the total of a shopping cart with one product" {
    let shoppingCart = { products = [ { key = "toothbrush" } ]}
    assertThat (Supermarket.total shoppingCart) (Ok 0.99)  
  }
  
  test "Should calculate the total of a shopping cart with some products" {
    let shoppingCart = { products = [ { key = "toothbrush" }; { key = "toothpaste" } ]}
    assertThat (Supermarket.total shoppingCart) (Ok 1.68) 
  }
  
  test "Should fail calculating the total of a shopping cart with an unknown product" {
    let shoppingCart = { products = [ { key = "unknown-product" } ]}
    
    let result = Supermarket.total shoppingCart
    
    assertThat result (Error (UnknownProduct "unknown-product"))  
  }
]