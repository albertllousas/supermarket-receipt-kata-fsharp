module SupermarketTests

open Expecto
open Supermarket

let assertThat actual expected = Expect.equal expected actual ""

[<Tests>]
let tests = testList "Supermarket tests" [

  test "Should calculate the total of an empty shopping cart" {
    let shoppingCart = { products = []}
    assertThat (Supermarket.total shoppingCart ) 0.0  
  }
]
