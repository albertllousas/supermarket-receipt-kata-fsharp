module SupermarketTests

open Expecto
open Supermarket.Types
open Supermarket.Defaults
open Supermarket

let assertThat actual expected = Expect.equal expected actual ""

let getPrice = getPriceFromMap (Map.ofList [
    ("toothpaste", 0.69)
    ("toothbrush", 0.99)
    ("apples", 1.99)
    ("rice", 2.49)
    ("cherry-tomatoes", 0.69)
  ]
)

let findDiscount = findDiscountFromMap (Map.ofList [] )

[<Tests>]
let tests = testList "Supermarket tests" [

  test "Should calculate the total of an empty shopping cart" {
    let shoppingCart = { items = []}
    
    let result = Receipt.total shoppingCart getPrice findDiscount
    
    assertThat result (Ok 0.0)  
  }
  
  test "Should calculate the total of a shopping cart with one product" {
    let shoppingCart = { items = [ { productKey = "toothbrush"; quantity = Units 1 } ]}
    
    let result = Receipt.total shoppingCart getPrice findDiscount
    
    assertThat result (Ok 0.99)  
  }
  
  test "Should calculate the total of a shopping cart with some products" {
    let shoppingCart = { items = [
      { productKey = "toothbrush"; quantity = Units 1 }; { productKey = "toothpaste"; quantity = Units 1 }
      ]}
    
    let result = Receipt.total shoppingCart getPrice findDiscount
    
    assertThat result (Ok 1.68) 
  }
  
  test "Should fail calculating the total of a shopping cart with an unknown product" {
    let shoppingCart = { items = [ { productKey = "unknown-product"; quantity = Units 1 } ]}
    
    let result = Receipt.total shoppingCart getPrice findDiscount
    
    assertThat result (Error (UnknownProduct "unknown-product"))  
  }
  
  test "Should calculate the total of a shopping cart priced by the kilogram" {
    let shoppingCart = { items = [ { productKey = "apples"; quantity = Kilograms 1.5 } ]}
    
    let result = Receipt.total shoppingCart getPrice findDiscount
    
    assertThat result (Ok 2.98) 
  }
  
  test "Should provide a receipt" {
    let shoppingCart = {
      items = [
        { productKey = "apples";          quantity = Kilograms 1.5 };
        { productKey = "toothbrush";      quantity = Units 1 }
        { productKey = "rice";            quantity = Units 1 };
        { productKey = "cherry-tomatoes"; quantity = Units 2 };
      ]
    }
    
    let result = Receipt.provide shoppingCart getPrice findDiscount
    
    let expectedReceipt = {
      lines = [
        { description = "apples";           quantity = Kilograms 1.5; price = 1.99; amount = 2.98 };
        { description = "toothbrush";       quantity = Units 1;       price = 0.99; amount = 0.99 };
        { description = "rice";             quantity = Units 1;       price = 2.49; amount = 2.49 };
        { description = "cherry-tomatoes";  quantity = Units 2;       price = 0.69; amount = 1.38 };
      ]
      discounts = []
      total = 7.84
    }
    assertThat result (Ok expectedReceipt) 
  }
  
  test "Should run a special deal: buy two toothbrushes, get one free." {
    let shoppingCart = { items = [ { productKey = "toothbrush"; quantity = Units 2 } ]}
    let findDiscount = findDiscountFromMap (Map.ofList [ ("toothbrush", BuyOneGetOneFree) ])
    
    let result = Receipt.provide shoppingCart getPrice findDiscount
    
    let expectedReceipt = {
      lines = [ { description = "toothbrush"; quantity = Units 2; price = 0.99; amount = 1.98 } ]
      discounts = [ { product = "toothbrush"; discount = BuyOneGetOneFree; amount = 0.99 } ]
      total = 0.99
    }
    assertThat result (Ok expectedReceipt) 
  }
  
  test "Should run a special deal: 20% discount on apples, normal price €1.99 per kilo." {
    let shoppingCart = { items = [ { productKey = "apples"; quantity = Kilograms 1.5 } ]}
    let findDiscount = findDiscountFromMap (Map.ofList [ ("apples", Percentage 20 ) ])
    
    let result = Receipt.provide shoppingCart getPrice findDiscount
    
    let expectedReceipt = {
      lines = [ { description = "apples"; quantity = Kilograms 1.5; price = 1.99; amount = 2.98 } ]
      discounts = [ { product = "apples"; discount = Percentage 20; amount = 0.59 } ]
      total = 2.39
    }
    assertThat result (Ok expectedReceipt) 
  }
]
