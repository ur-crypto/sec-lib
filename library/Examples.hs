module Examples where
import Types
import Utils
import Ops as O
import Data.Int
import Data.Bits
import Data.Array
import Data.Array.ST
import Control.Monad.ST
import Prelude hiding ((&&), (||), ifThenElse,ifThenElses)


test8 :: Int8
test8 = 35

testb8 :: Int8
testb8 = 74


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


--andShift :: SecureFunction a
--andShift xs ys = (O.shiftL 1 xs) O..&. ys

numCmps :: [Node a] -> [Node a] -> [Node a]
numCmps as bs = (Constant False):[]
{-numCmps as bs = imp as bs
    where
    imp :: [Node a] -> [Node a] -> Node a
    imp [n1] [n2] = 
           let nbool = Not n2 in
           n1 && nbool
    imp (n1:n1s) (n2:n2s) =
           let nbool = Not n2 
               mbool = Not n1 
               l1 = length n1s
               l2 = length n2s in
               case (l1 > l2,l1 < l2) of
                    (True,_)      -> n1 || (imp n1s (n2:n2s))
                    (False,False) -> ifThenElse ( n1 && nbool) n1 (ifThenElse (mbool && n2) n1 (imp n1s n2s))
                    (False,True)  -> n2 || (imp (n1:n1s) n2s)
    imp _ _ = error "Bad args for imp"
-}

numCmp :: SecureFunction a
numCmp as bs = [imp as bs]
    where
    imp :: [Node a] -> [Node a] -> Node a
    imp [n1] [n2] = 
           let nbool = Not n2 in
           n1 && nbool
    imp (n1:n1s) (n2:n2s) =
           let nbool = Not n2 
               mbool = Not n1 in
           ifThenElse ( n1 && nbool) n1 (ifThenElse (mbool && n2) n1 (imp n1s n2s))
    imp _ _ = error "Bad args for imp"

numEqs :: [Node a] -> [Node a] -> Node a          
numEqs n1 n2 = foldl1 (&&) (zipWith bij n1 n2)

numEq :: SecureFunction a
numEq n1 n2 = [foldl1 (&&) (zipWith bij n1 n2)]

hammingDist :: SecureFunction a
hammingDist n1 n2 = let difference = xor n1 n2 in
                        let (len,distance) = hammingWeight 64  difference in
                            (Constant False:[])++distance


hammingWeight :: Int -> [Node a1] -> (Int, [Node a1])
hammingWeight p n = 
    case (p>1) of
         (True)     ->    let (leftHalf,rightHalf) = splitAt (quot p 2) n in
                               let (lenleft,subleft) = hammingWeight (quot p 2) leftHalf
                                   (lenright,subright) = hammingWeight (quot p 2) rightHalf in
                                    let (len,(carry,subTotal)) = addIntFP 6 lenleft subleft lenright subright in
                                          (len+1,((carry:[])++subTotal))
         (False)    ->    (1,n)

ourBool (n1:n1s) = case n1 of
                       (True)   ->    ((Constant True):[]) ++ (ourBool n1s)
                       (False)  ->    ((Constant False):[]) ++ (ourBool n1s)
ourBool [] = []

{-
editDist :: SecureFunction a
editDist xs ys =  let dist = initDist (length ys) in
                         editDistVec [Constant False] [Constant False] dist xs ys

initDist :: Num -> [[Node a]]
initDist a = case (a>1) of
                 (True)     ->    let dist = initDist(a-1) in 
                                      dist++[bitsToBools (fromIntegral (a :: Int) :: Int8))]
                 (False)    ->    [Constant True]

editDistVec :: [Node a] -> [Node a] -> [[Node a]] -> SecureFunction a
editDist d0 d1 (d:ds) (x:xs) (y:ys) = let v1 = ((xor x y)+d0) in
                                      let v2 = (if' (numCmp d v1) v1 d) in
                                      let v3 = (if' (numCmp d1 v2) v2 d1) in
                                          editDist 
-}

editDistance :: SecureFunction a 
editDistance xs ys = table ! (m,n)
    where
    m     = 7 :: Int
    n     = 7 :: Int
    --(m,n) = (length xs, length ys)
    x     = array (1,m) (zip [1..] xs)
    y     = array (1,n) (zip [1..] ys)
 
    --table :: Array (Int,Int) [Node a]
    table = array bnds [(ij, dist ij) | ij <- range bnds]
    bnds  = ((0,0),(m,n))
    --bnds  = ((0,0),(m,n))
    dist (0,0) = [Constant False] 
    --dist (0,j) = let (l1,(b1,a1)) = addIntFP 4 (length (table ! (0,j-1))) (table ! (0,j-1)) 1 ((Constant True):[]) in (b1:[])++a1
    --dist (i,0) = let (l1,(b1,a1)) = addIntFP 4 (length (table ! (i-1,0))) (table ! (i-1,0)) 1 ((Constant True):[]) in (b1:[])++a1
    dist (0,j) = ourBool (bits2Bools (fromIntegral (j :: Int) :: Int8))
    dist (i,0) = ourBool (bits2Bools (fromIntegral (i :: Int) :: Int8))
    dist (i,j) = let (li,(carry,intermed)) = addIntFP 4 (length (table ! (i-1,j-1))) (table ! (i-1,j-1)) 1 ((Constant True):[]) in 
                     let result = if' (numCmps (table ! (i-1,j)) (table ! (i,j-1))) (table ! (i,j-1))
                                          (if' (numCmps (table ! (i-1,j-1)) ((carry:[])++intermed)) ((carry:[])++intermed) (table ! (i-1,j))) in
                              let (l1,(b1,a1)) = addIntFP 4 (length result) result 1 ((Constant True):[]) in
                              (b1:[])++a1
                     -- ifThenElses (numCmps ((b1:[])++a1) ((b2:[])++a2)) ((b2:[])++a2) ((b1:[])++a1)
    --dist (i,j) = a1 --ifThenElses (numCmps a1 a2) a2 (ifThenElses (numCmps a1 a3) a3 a1)
                    -- a3 = ifThenElses (Gate XOR (x ! i) (y ! i)) ab3 aa3
                      --   where
                        -- aa3 = table ! (i-1,j-1)
                        -- (la3,(bb3,ab3)) = addIntFP 8 (length (table ! (i-1,j-1))) (table ! (i-1,j-1)) 2 ((Constant True):(Constant False):[]) 

-- minimum [table ! (i-1,j) + 1, table ! (i,j-1) + 1,
--        if x ! i == y ! j then table ! (i-1,j-1) else 1 + table ! (i-1,j-1)]


ueand :: [Node a] -> [Node a] -> [Node a]
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

urexor :: [Node a] -> [Node a] -> [Node a]
ureand :: [Node a] -> [Node a] -> [Node a]
ureand n1 n2 = reverse (ueand (reverse n1) (reverse n2))
urexor n1 n2 = reverse (uexor (reverse n1) (reverse n2))

uexor :: [Node a] -> [Node a] -> [Node a]
uexor (n1:n1s) [] = (n1:n1s)
uexor [] (n1:n1s) = (n1:n1s) 
--uexor [] [n1] = n1:[]
--uexor [n1] [] = n1:[]
uexor [] [] = []
uexor [n1] [n2] = (b_xor n1 n2):[]
uexor (n1:n1s) (n2:n2s) = let reslt = uexor n1s n2s in
                              (((b_xor n1 n2):[])++reslt)

hammingWt :: Int -> [Node a1] -> (Int, [Node a1])
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
                                          let (len,(carry,subTotal)) = addIntFP 6 mx fsummand (mx+1) ssummand in
                                              (len+1,((carry:[])++subTotal))
         (False,True)    -> let (fb:[sb]) = n in (2,((fb && sb):((b_xor) fb sb):[])) 
         (False,False)   -> (1,n)

boolsEditDistance :: [Bool] -> [Bool] -> Int
boolsEditDistance sa sb = last $ foldl transform [0..length sa] sb
    where
        transform xs@(x:xs') c = scanl compute (x+1) (zip3 sa xs xs') 
            where
                compute z (c', x, y) = minimum [y+1, z+1, x + fromEnum (c' /= c)]

levenshtein2 :: SecureFunction a
levenshtein2 sa sb = last $ foldl transform (map O.num2Const [0..length sa]) sb
    where
        transform xs@(x:xs') c = scanl compute (x+(O.num2Const 1)) (zip3 sa xs xs') 
            where
                compute z (c', x, y) = foldl1 cmp [y+(O.num2Const 1), z+(O.num2Const 1), x + (extendBy 15 $ [O.b_xor c' c])]
                    where
                        cmp a b = O.if' (a O.<. b) a b


--edistance :: Eq a => [a] -> [a] -> Int
edistance :: SecureFunction a
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
                                  let c = if' (extendBy 1 $ [O.b_xor (s'!(i-1)) (t'! (j-1))])
                                           (O.num2Const 1)  (O.num2Const 0)
                                  x <- readArray m (i-1,j)
                                  y <- readArray m (i,j-1)
                                  z <- readArray m (i-1,j-1)
                                  writeArray m (i,j) $ foldl1 cmp [x+(O.num2Const 1), y+(O.num2Const 1), z+c ]
                return m

cmp a b = O.if' (a O.<. b) a b
for_ xs f =  mapM_ f xs


edist :: SecureFunction a
edist s1 s2 = iter s1 s2 ls2 where
               (_,ls2) = (initDist (length s2))
               iter (c:cs) s2 row@(e:es) =
                       iter cs s2 (e' : rest e' c s2 row) where
                               e' = e + (O.num2Const 1)
               iter [] _ row = last row
               iter _ _ _ = error "iter (distance): unexpected arguments"
               rest e c (c2:c2s) (e1:es@(e2:es')) =
                       seq k (k : rest k c c2s es) where
                               k = (cmp (e1 + (if' [(b_xor c c2)] (O.num2Const 0) (O.num2Const 1))) $
                                       cmp (e+(O.num2Const 1)) (e2+(O.num2Const 1)))
               rest _ _ [] _ = []
               rest _ _ _ _ = error "rest (distance): unexpected arguments"


initDist :: Int -> ([Node a],[[Node a]])
initDist a = case (a>0) of
                 (True)     ->    let (lst,dist) = initDist(a-1) in
                                      let nxtt = lst+(O.num2Const 1) in 
                                          (nxtt,(dist++[nxtt]))
                 (False)    ->    ([Constant False],[[Constant False]])
