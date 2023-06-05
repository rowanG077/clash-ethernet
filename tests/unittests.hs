import Prelude

import Test.Tasty

import qualified Test.Cores.Ethernet.Ethernet
import qualified Test.Cores.Ethernet.CRC

main :: IO ()
main = defaultMain $ testGroup "."
  [ Test.Cores.Ethernet.Ethernet.tests,
    Test.Cores.Ethernet.CRC.tests
  ]
