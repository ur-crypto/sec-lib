import Crypto

main :: IO ()
main = 
    do 
        let l1 = KeyPair $ 0 1
        let l2 = KeyPair $ 2 3
        putStr $ plusTable $ l1 l2
