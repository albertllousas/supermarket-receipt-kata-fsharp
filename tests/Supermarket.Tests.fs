module SupermarketTests

open Expecto
open Supermarket

let assertThat actual expected = Expect.equal expected actual ""

let catalog = { prices = Map.ofList [ ("toothpaste", 0.69); ("toothbrush", 0.99); ("apples", 1.99) ] }

[<Tests>]
let tests = testList "Supermarket tests" [

  test "Should calculate the total of an empty shopping cart" {
    let shoppingCart = { items = []}
    
    let result = Supermarket.total shoppingCart catalog
    
    assertThat result (Ok 0.0)  
  }
  
  test "Should calculate the total of a shopping cart with one product" {
    let shoppingCart = { items = [ { productKey = "toothbrush"; quantity = Units 1 } ]}
    
    let result = Supermarket.total shoppingCart catalog
    
    assertThat result (Ok 0.99)  
  }
  
  test "Should calculate the total of a shopping cart with some products" {
    let shoppingCart = { items = [
      { productKey = "toothbrush"; quantity = Units 1 }; { productKey = "toothpaste"; quantity = Units 1 }
      ]}
    
    let result = Supermarket.total shoppingCart catalog
    
    assertThat result (Ok 1.68) 
  }
  
  test "Should fail calculating the total of a shopping cart with an unknown product" {
    let shoppingCart = { items = [ { productKey = "unknown-product"; quantity = Units 1 } ]}
    
    let result = Supermarket.total shoppingCart catalog
    
    assertThat result (Error (UnknownProduct "unknown-product"))  
  }
  
  test "Should calculate the total of a shopping cart priced by the kilogram" {
    let shoppingCart = { items = [ { productKey = "apples"; quantity = Kilograms 1.5 } ]}
    
    let result = Supermarket.total shoppingCart catalog
    
    assertThat result (Ok 2.98) 
  }
]