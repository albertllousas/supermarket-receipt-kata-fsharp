module SupermarketTests

open Expecto
open Supermarket

let assertThat actual expected = Expect.equal expected actual ""

[<Tests>]
let tests = testList "Supermarket tests" [

  test "Should work" {
    assertThat 1 1
  }
]