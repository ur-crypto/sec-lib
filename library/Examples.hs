module Examples where
import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST
import           Data.Bits
import           Data.Int
import           Data.List.Split
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

testmax :: Int64
testmax = 18446744073709551615

testmin :: Int64
testmin = 0

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

numEqs :: [Literal] -> [Literal] -> Literal
numEqs n1 n2 = foldl1 (&&) (zipWith bij n1 n2)

numEq :: SecureFunction
numEq n1 n2 = [foldl1 (&&) (zipWith bij n1 n2)]

hammingDistEff :: [Literal] -> [Literal] -> [Literal]
hammingDistEff n1 n2 = let difference = xor n1 n2 in
                           recAdd 1 (map (\[x,y] -> (addIntEff 1 [x] [y])) . chunksOf 2 $ difference)
      where
          recAdd :: Int -> [[Literal]] -> [Literal]
          recAdd p (n:ns) = case ((length ns) > 0) of
                 (True) -> recAdd (1+p) (map (\[x,y] -> (addIntEff (1+p) x y)) . chunksOf 2 $ (n:ns))
                 (False) -> n
          recAdd p [n] = n

hammingDist :: SecureFunction
hammingDist n1 n2 = let difference = xor n1 n2 in
                        let distance = hammingWeight (length n1) difference in
                            (Constant False:[])++distance

{-
hammingWeightEff :: Int -> [Literal] -> [Literal]
hammingWeightEff p n =
    case (p>1) of
         (True)     ->    let (leftHalf,rightHalf) = splitAt (quot p 2) n in
                               let (lenleft,subleft) = hammingWeight (quot p 2) leftHalf
                                   (lenright,subright) = hammingWeight (quot p 2) rightHalf in
                                    let (len,(carry,subTotal)) = addIntFP 6 subleft subright in
                                          (len+1,((carry:[])++subTotal))
         (False)    ->    (1,n)
-}

hammingWeight :: Int -> [Literal] -> [Literal]
hammingWeight p n =
    case (p>1) of
         (True)     ->    let (leftHalf,rightHalf) = splitAt (quot p 2) n in
                               let subleft = hammingWeight (quot p 2) leftHalf
                                   subright = hammingWeight (quot p 2) rightHalf in
                                       --addIntFP p subleft subright
                                   let (_,(carry,subTotal)) = addIntFP 6 subleft subright in
                                          ((carry:[])++subTotal)
         (False)    ->    n

ourBool (n1:n1s) = case n1 of
                       (True)   ->    ((Constant True):[]) ++ (ourBool n1s)
                       (False)  ->    ((Constant False):[]) ++ (ourBool n1s)
ourBool [] = []




logCeil :: Int -> Int
logCeil i = case (i>1) of
            (True) ->  ceiling ( logBase 2 (fromIntegral (i+1)))
            (False) -> 1

editDist :: Int -> Int -> SecureFunction
editDist i j xs ys =  let
                        xss = (take i xs)
                        yss = (take j ys) in
                        let prevCol = initDistEff (length yss) in
                         editDistEff 1 [Constant False] prevCol xss yss

editDistEff :: Int -> [Literal] -> [[Literal]] -> SecureFunction
editDistEff i topValue es (x:xs) ys =
                  let prev = addIntEff (logCeil i) topValue [Constant True] in
                         let updatedColumn = columnCalc i 1 [prev]  es x ys in
                              editDistEff (i+1) prev updatedColumn xs ys

editDistEff _ _ es [] _ = (last es)

columnCalc :: Int -> Int -> [[Literal]]  -> [[Literal]] -> Literal -> [Literal] -> [[Literal]]
columnCalc i j curColumn  (e1:es@(e2:es')) x (y:ys) =
        let addValue = O.bXor x y
            prev = last curColumn in
                  let tempMatch = addIntEff (logCeil (max i j)) e1 [addValue]
                      firstCompare = cmpp prev e2 in
                   let [secondCompare] = numCmps firstCompare tempMatch in
                   let secondMatch = ifList secondCompare tempMatch firstCompare in
                    let currentValue = (addIntEff (logCeil (max i j)) secondMatch [((O.not secondCompare) || (secondCompare && addValue))]) in
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

ifWithPadding :: [Literal] -> [Literal] -> [Literal]
ifWithPadding (x:xs) (y:ys) = (x || y) : ifWithPadding xs ys
ifWithPadding []     ys     =  ys
ifWithPadding xs     []     =  xs

ifzip :: Literal -> SecureFunction
ifzip cnd xs ys = let nbool = (O.not cnd) in
       let firstList = map (&& cnd) xs
           secondList = map (&& nbool) ys in
           reverse (ifWithPadding firstList secondList)

{-
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
-}

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
           ifzip reslt b a
cmp a b = O.if' (a O.<. b) a b
for_ xs f =  mapM_ f xs

edisteff xs ys = let xss = (take 2 xs)
                     yss = (take 2 ys) in
                     edist xss yss

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

initDistEff :: Int -> [[Literal]]
initDistEff a = [Constant False]: (imp 1 a [Constant False])
    where
    imp :: Int -> Int -> [Literal] -> [[Literal]]
    imp p a currentNum = case (p > a) of
            (False) -> let nextNum = addIntEff (logCeil p) currentNum [Constant True] in
                       nextNum : (imp (p+1) a nextNum)
            (True) -> []

addIntEff :: Int -> [Literal] -> [Literal] -> [Literal]
addIntEff p n1s n2s = let l1 = length n1s
                          l2 = length n2s in
                          let m1s = take (min (max l1 l2) p) (  reverse ((replicate (max 0 (l2 - l1)) (Constant False))++n1s))
                              m2s = take (min (max l1 l2) p) (  reverse ((replicate (max 0 (l1 - l2)) (Constant False))++n2s)) in
                              let generate = m1s .&. m2s
                                  propogate = xor m1s m2s in
                                    imp p (Constant False) propogate generate
    where
    imp :: Int -> Literal -> [Literal] -> [Literal] -> [Literal]
    imp p carry (n1:n1s) (n2:n2s) = (imp (p-1) (O.bXor n2 (n1 && carry)) n1s n2s) ++ [(O.bXor carry n1)]
    imp 0 carry _ _ = [carry]
    imp p carry [] [] = [carry]
    --imp p carry (n1:n1s) [] = (imp (p-1) (n1 && carry) n1s []) ++ [(O.bXor n1 carry)]
    --imp p carry [] n2s = imp p carry n2s []
    
 
myAdder :: Int -> [Literal] -> [Literal] -> [Literal]
myAdder p (n1:n1s) (n2:n2s) = (O.bXor n1 n2): imp (p-1) (n1 && n2) n1s n2s
       where
          imp :: Int -> Literal -> [Literal] -> [Literal] -> [Literal]
          imp p carry (n1:n1s) (n2:n2s) = (O.bXor (O.bXor n1 n2) carry): imp (p-1) (O.bXor (O.bXor (n1 && n2) (n1 && carry)) (n2 && carry)) n1s n2s 
          imp 0 _ _ _ = []
          imp p carry (n1:n1s) [] = (O.bXor n1 carry): imp (p-1) (n1 && carry) n1s []
          imp p carry [] (n1:n1s) = (O.bXor n1 carry): imp (p-1) (n1 && carry) n1s []
          imp p carry [] [] = [carry]


myCmp :: [Literal] -> [Literal] -> Literal
myCmp (n1:n1s) (n2:n2s) = imp ((O.bXor n1 n2) && n1) n1s n2s
   where
        imp :: Literal -> [Literal] -> [Literal] -> Literal
        imp reslt (n1:n1s) (n2:n2s) = imp (ifThenElse (O.bXor n1 n2) n1 reslt) n1s n2s
        imp reslt (n1:n1s) [] = imp (reslt || n1) n1s []
        imp reslt [n1] [] = reslt || n1
        imp reslt [] (n1:n1s) = imp (reslt && (O.not n1)) [] n1s 
        imp reslt [] [n1] = reslt && (O.not n1) 
        imp reslt [] [] = reslt
                                 
myIf :: Literal -> [Literal] -> [Literal] -> [Literal]
myIf bool (n1:n1s) (n2:n2s) = let nbool = O.not bool in
                                  imp bool nbool (n1:n1s) (n2:n2s)
   where
        imp :: Literal -> Literal -> [Literal] -> [Literal] -> [Literal]
        imp bool nbool (n1:n1s) (n2:n2s) = ((n1 && bool) || (n2 && nbool)): (imp bool nbool n1s n2s)
        imp bool nbool (n1:n1s) [] = (n1 && bool) : (imp bool nbool n1s [])
        imp bool nbool [] (n1:n1s) = (n1 && nbool) : (imp bool nbool [] n1s)
        imp bool nbool [] []       = []


myInitDist :: Int -> [[Literal]]
myInitDist a = [Constant False]: (imp 1 a [Constant False])
    where
    imp :: Int -> Int -> [Literal] -> [[Literal]]
    imp p a currentNum = case (p > a) of
            (False) -> let nextNum = myAdder (logCeil p) currentNum [Constant True] in
                       nextNum : (imp (p+1) a nextNum)
            (True) -> []

myEditDist :: Int -> Int -> [Literal] -> [Literal] -> [Literal]
myEditDist i j xs ys = let prevCol = myInitDist j in
                           myEditDistEff 1 i j [Constant False] prevCol xs ys

myEditDistEff :: Int -> Int -> Int ->[Literal] -> [[Literal]] -> [Literal] -> [Literal] -> [Literal] 
myEditDistEff ic i j topValue es (x:xs) ys =
              case (ic <= i) of
                 (True)  ->  let prev = myAdder (logCeil i) topValue [Constant True] in
                             let updatedColumn = myColumnCalc i j 1 [prev] prev  es x ys in
                                 myEditDistEff (ic+1) i j prev  updatedColumn xs ys
                 (False) ->  (last es)
myEditDistEff _ _ _ _ es [] _ = (last es)

myColumnCalc :: Int -> Int -> Int -> [[Literal]]  -> [Literal] -> [[Literal]] -> Literal -> [Literal] -> [[Literal]]
myColumnCalc i j jc curColumn prev  (e1:es@(e2:es')) x (y:ys) =
     case (jc <= j) of
       (True) ->  let addValue = O.bXor x y in
                      let tempMatch = myAdder (logCeil (max i j)) e1 [addValue] 
                          firstCompare = myIf (myCmp prev e2) e2 prev in
                          let secondCompare = myCmp firstCompare tempMatch in
                          let secondMatch = myIf secondCompare tempMatch firstCompare in
                          let currentValue = (myAdder (logCeil (max i j)) secondMatch [((O.not secondCompare) || (secondCompare && addValue))]) in
                          myColumnCalc i j (jc+1) (curColumn++[currentValue]) currentValue  es x ys
       (False) -> curColumn
myColumnCalc _ _ _ curColumn _ _ _ _ = curColumn


