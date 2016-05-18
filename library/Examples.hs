module Examples where
import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST
import           Data.Bits
import           Data.Int
import           Data.LargeWord
import           Data.Word
import           Debug.Trace
import           Ops              as O
import           Prelude          hiding (ifThenElse, ifThenElses, (&&), (||))
import           Types
import           Utils
test8 :: Int8
test8 = 35

testb8 :: Int8
testb8 = 74

test4 :: SecureNum
test4 = [Constant True, Constant True, Constant False, Constant True]

testb4 :: SecureNum
testb4 = [Constant True, Constant False, Constant True, Constant True]

test16 :: Int16
test16 = 125

testb16 :: Int16
testb16 = 200

test32 :: Int32
test32 = 39393


testb32 :: Int32
testb32 = 120200

testb64 :: Int64
testb64 = 744073709551616

test64 :: Int64
test64 = 395648674974903

test128 :: Word128
test128 = LargeKey (14375142357289042 :: Word64) (129467138759104 :: Word64)

testb128 :: Word128
testb128 = LargeKey (384902357290471 :: Word64) (194781841240187 :: Word64)


--andShift :: SecureFunction
--andShift xs ys = (O.shiftL 1 xs) O..&. ys

numCmps :: [Literal] -> [Literal] -> [Literal]
--numCmps as bs = (Constant False):[]
numCmps as bs = [(imp as bs)]
    where
    imp :: [Literal] -> [Literal] -> Literal
    imp [n1] [n2] = n1 && O.not n2
    imp (n1:n1s) (n2:n2s) =
           let l1 = length n1s
               l2 = length n2s in
               case (l1 > l2,l1 < l2) of
                    (True,_)      -> n1 || (imp n1s (n2:n2s))
                    (False,False) -> ifThenElse (O.bXor n1 n2) n1 (imp n1s n2s)
                    (False,True)  -> (O.not n2) && (imp (n1:n1s) n2s)
    imp _ _ = error "Bad args for imp"


numCmp :: SecureFunction
numCmp as bs = [imp as bs]
    where
    imp :: [Literal] -> [Literal] -> Literal
    imp [n1] [n2] =
           let nbool = O.not n2 in
           n1 && nbool
    imp (n1:n1s) (n2:n2s) =
           let nbool = O.not n2
               mbool = O.not n1 in
           ifThenElse ( n1 && nbool) n1 (ifThenElse (mbool && n2) n1 (imp n1s n2s))
    imp _ _ = error "Bad args for imp"

numEqs :: [Literal] -> [Literal] -> Literal
numEqs n1 n2 = foldl1 (&&) (zipWith bij n1 n2)

numEq :: SecureFunction
numEq n1 n2 = [foldl1 (&&) (zipWith bij n1 n2)]

hammingDist :: SecureFunction
hammingDist n1 n2 = let difference = xor n1 n2 in
                        let (len,distance) = hammingWeight (length n1) difference in
                            (Constant False:[])++distance


hammingWeight :: Int -> [Literal] -> (Int, [Literal])
hammingWeight p n =
    case (p>1) of
         (True)     ->    let (leftHalf,rightHalf) = splitAt (quot p 2) n in
                               let (lenleft,subleft) = hammingWeight (quot p 2) leftHalf
                                   (lenright,subright) = hammingWeight (quot p 2) rightHalf in
                                    let (len,(carry,subTotal)) = addIntFP 6 subleft subright in
                                          (len+1,((carry:[])++subTotal))
         (False)    ->    (1,n)

ourBool (n1:n1s) = case n1 of
                       (True)   ->    ((Constant True):[]) ++ (ourBool n1s)
                       (False)  ->    ((Constant False):[]) ++ (ourBool n1s)
ourBool [] = []

logCeil :: Int -> Int
logCeil i = case (i>1) of
            (True) -> ceiling ( logBase 2 (fromIntegral i))
            (False) -> 1

editDist :: Int -> Int -> SecureFunction
editDist i j xs ys =  let
                        xss = (take i xs)
                        yss = (take j ys) in
                      let (_,prevCol) = initDist (length yss) in
                         editDistEff 1 [Constant False] prevCol xss yss

editDistEff :: Int -> [Literal] -> [[Literal]] -> SecureFunction
editDistEff i topValue es (x:xs) ys =
                  let (_,(carry,subTotal)) = addIntFP (logCeil i) topValue [Constant True] in
                             let prev = [carry]++subTotal in
                               let updatedColumn = columnCalc i 1 [prev]  es x ys in
                                  editDistEff (i+1) prev updatedColumn xs ys

editDistEff _ _ es [] _ = (last es)

columnCalc :: Int -> Int -> [[Literal]]  -> [[Literal]] -> Literal -> [Literal] -> [[Literal]]
columnCalc i j curColumn  (e1:es@(e2:es')) x (y:ys) =
        let addValue = O.bXor x y
            prev = last curColumn in
                  let (_,(carry,subTotal)) = addIntFP (logCeil (max i j)) e1 [addValue] in
                   let tempMatch = [carry]++subTotal
                       firstCompare = cmpp prev e2 in
                   let [secondCompare] = numCmps firstCompare tempMatch in
                   let secondMatch = ifList secondCompare tempMatch firstCompare in
                    let (_,(carry2,subTotal2)) = (addIntFP (logCeil (max i j)) secondMatch [((O.not secondCompare) || (secondCompare && addValue))]) in
                    let currentValue = [carry2]++subTotal2 in
                      columnCalc i (j+1) (curColumn++[currentValue])  es x ys
columnCalc _ _ curColumn _ _ _ = curColumn

ifList :: Literal -> SecureFunction
ifList cnd xs ys = let nbool = (O.not cnd) in
       ifrList [] cnd nbool (reverse xs) (reverse ys)

ifrList :: [Literal] -> Literal -> Literal -> SecureFunction
ifrList l bool nbool (x:xs) (y:ys) =
       let val = (bool && x) || (nbool && y) in
           ifrList ([val]++l) bool nbool xs ys
ifrList l bool nbool [] (y:ys) =
       let val = (nbool && y) in
           ifrList ([val]++l) bool nbool [] ys
ifrList l bool nbool (x:xs) []  =
       let val = (bool && x) in
           ifrList ([val]++l) bool nbool xs []
ifrList l _ _ _ _ = l


editDistance :: SecureFunction
editDistance xs ys = table ! (m,n)
    where
    m     = 3 :: Int
    n     = 3 :: Int
    x     = array (1,m) (zip [1..] xs)
    y     = array (1,n) (zip [1..] ys)

    table = array bnds [(ij, dist ij) | ij <- range bnds]
    bnds  = ((0,0),(m,n))
    dist (0,0) = [Constant False]
    dist (0,j) = ourBool (bits2Bools (fromIntegral (j :: Int) :: Int8))
    dist (i,0) = ourBool (bits2Bools (fromIntegral (i :: Int) :: Int8))
    dist (i,j) = let (li,(carry,intermed)) = addIntFP 4 (table ! (i-1,j-1)) ((Constant True):[]) in
                     let result = if' (numCmps (table ! (i-1,j)) (table ! (i,j-1))) (table ! (i,j-1))
                                          (if' (numCmps (table ! (i-1,j-1)) ((carry:[])++intermed)) ((carry:[])++intermed) (table ! (i-1,j))) in
                              let (l1,(b1,a1)) = addIntFP 4 result ((Constant True):[]) in
                              (b1:[])++a1

ueand :: [Literal] -> [Literal] -> [Literal]
ueand (n1:n1s) [] = let reslt = ueand n1s [] in
                              (((Constant False):[])++reslt)
ueand [] (n1:n1s) = let reslt = ueand n1s [] in
                              (((Constant False):[])++reslt)
--ueand [] [n1] = (Constant False):[]
--ueand [n1] [] = (Constant False):[]
ueand [] [] = []
ueand [n1] [n2] = (n1 && n2):[]
ueand (n1:n1s) (n2:n2s) = let reslt = ueand n1s n2s in
                              (((n1 && n2):[])++reslt)

urexor :: [Literal] -> [Literal] -> [Literal]
ureand :: [Literal] -> [Literal] -> [Literal]
ureand n1 n2 = reverse (ueand (reverse n1) (reverse n2))
urexor n1 n2 = reverse (uexor (reverse n1) (reverse n2))

uexor :: [Literal] -> [Literal] -> [Literal]
uexor (n1:n1s) [] = (n1:n1s)
uexor [] (n1:n1s) = (n1:n1s)
--uexor [] [n1] = n1:[]
--uexor [n1] [] = n1:[]
uexor [] [] = []
uexor [n1] [n2] = (O.bXor n1 n2):[]
uexor (n1:n1s) (n2:n2s) = let reslt = uexor n1s n2s in
                              (((O.bXor n1 n2):[])++reslt)

hammingWt :: Int -> [Literal] -> (Int, [Literal])
hammingWt p n =
    case (p>2,p>1) of
         (True,_)        -> let (leftThird,rightHalf) = splitAt (quot p 3) n in
                             let (midThird,rightThird) = splitAt (quot p 3) rightHalf in
                                  let (lenleft,subleft) = hammingWt (quot p 3) leftThird
                                      (lenmid,submid) = hammingWt (quot p 3) midThird
                                      (lenright,subright) = hammingWt (p-2*(quot p 3)) rightThird in
                                      let fsummand = (urexor) ((urexor) subleft submid) subright
                                          ssummand = ((urexor) ((urexor) (ureand subleft submid) (ureand submid  subright)) (ureand subleft subright))++((Constant False):[])
                                          mx = maximum((lenleft:lenright:lenmid:[])) in
                                          let (len,(carry,subTotal)) = addIntFP 6 fsummand ssummand in
                                              (len+1,((carry:[])++subTotal))
         (False,True)    -> let (fb:[sb]) = n in (2,((fb && sb):((O.bXor) fb sb):[]))
         (False,False)   -> (1,n)

boolsEditDistance :: [Bool] -> [Bool] -> Int
boolsEditDistance sa sb = last $ foldl transform [0..length sa] sb
    where
        transform xs@(x:xs') c = scanl compute (x+1) (zip3 sa xs xs')
            where
                compute z (c', x, y) = minimum [y+1, z+1, x + fromEnum (c' /= c)]

levenshtein2 :: SecureFunction
levenshtein2 sa sb = last $ foldl transform (map O.num2Const [0..length sa]) sb
    where
        transform xs@(x:xs') c = scanl compute (x+(O.num2Const 1)) (zip3 sa xs xs')
            where
                compute z (c', x, y) = foldl1 cmp [y+(O.num2Const 1), z+(O.num2Const 1), x + (extendBy 7 $ [O.bXor c' c])]
                    where
                        cmp a b = O.if' (a O.<. b) a b


--edistance :: Eq a => [a] -> [a] -> Int
edistance :: SecureFunction
edistance s t = d ! (ls , lt)
    where s' = array (0,ls) [ (i,x) | (i,x) <- zip [0..] s ]
          t' = array (0,lt) [ (i,x) | (i,x) <- zip [0..] t ]
          --ls = 4
          --lt = 4
          ls = length s
          lt = length t
          (l,h) = ((0,0),(length s,length t))
          d = runSTArray $ do
                m <- newArray (l,h) 0
                for_ [0..ls] $ \i -> writeArray m (i,0) (ourBool (bits2Bools (fromIntegral (i :: Int) :: Int8)))
                for_ [0..lt] $ \j -> writeArray m (0,j) (ourBool (bits2Bools (fromIntegral (j :: Int) :: Int8)))
                for_ [1..lt] $ \j -> do
                              for_ [1..ls] $ \i -> do
                                  let c = if' (extendBy 1 $ [O.bXor (s'!(i-1)) (t'! (j-1))])
                                           (O.num2Const 1)  (O.num2Const 0)
                                  x <- readArray m (i-1,j)
                                  y <- readArray m (i,j-1)
                                  z <- readArray m (i-1,j-1)
                                  writeArray m (i,j) $ foldl1 cmp [x+(O.num2Const 1), y+(O.num2Const 1), z+c ]
                return m

cmpp a b = let [reslt] = numCmps a b in
           ifList reslt b a
cmp a b = O.if' (a O.<. b) a b
for_ xs f =  mapM_ f xs


edist :: SecureFunction
edist s1 s2 = iter s1 s2 ls2 where
               (_,ls2) = (initDist (length s2))
               iter (c:cs) s2 row@(e:es) =
                       iter cs s2 (e' : rest e' c s2 row) where
                               e' = e + (O.num2Const 1)
               iter [] _ row = last row
               iter _ _ _ = error "iter (distance): unexpected arguments"
               rest e c (c2:c2s) (e1:es@(e2:es')) =
                       seq k (k : rest k c c2s es) where
                               k = (cmpp (e1 + (if' [(O.bXor c c2)] (O.num2Const 0) (O.num2Const 1))) $
                                       cmpp (e+(O.num2Const 1)) (e2+(O.num2Const 1)))
               rest _ _ [] _ = []
               rest _ _ _ _ = error "rest (distance): unexpected arguments"


initDist :: Int -> ([Literal],[[Literal]])
initDist a = case (a>0) of
                 (True)     ->    let (lst,dist) = initDist(a-1) in
                                    let (_,(carry,subTotal)) = addIntFP 2 lst [Constant True] in
                                      let nxtt = [carry]++subTotal in
                                          (nxtt,(dist++[nxtt]))
                 (False)    ->    ([Constant False],[[Constant False]])
