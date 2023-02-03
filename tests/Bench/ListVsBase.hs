{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -ddump-simpl -dsuppress-all -ddump-to-file #-}

--
-- Benchmark tool.
-- Compare a function against equivalent code from other libraries for
-- space and time.
--
import Bench.Utils hiding (force)

import qualified Data.List        as L -- theirs
import qualified Data.List.Stream as S -- ours 

import Data.Char

import Test.Tasty.Bench
import Control.DeepSeq

main :: IO ()
main = do
    -- initialise
    let input = Input (take 1000 string) string string2 (splitEvery 1000 string2)
    input <- return $! force input

    putStrLn "Benchmarking Data.List.Stream <=> Data.List"
    putStrLn "==========================================="
    putStrLn ""

    putStrLn $ "# Size of test data: " ++ show ((floor $ (fromIntegral (length string)) / 1024) :: Int) ++ "k"
    putStrLn "#List\t List.Stream"
    putStrLn ""

    defaultMain (tests input)

------------------------------------------------------------------------

tests input =
    let 
        mkBench :: (NFData b, Ap a) => String -> (a -> b) -> Benchmark
        mkBench name f = bench name $ nf (app f) input
        benchL :: (NFData b, Ap a) => (a -> b) -> Benchmark
        benchL = mkBench "List"
        benchS :: (NFData b, Ap a) => (a -> b) -> Benchmark
        benchS = mkBench "Stream"
        
        -- [NOTE] Works if groupTitle uniquely identifies the benchmark group.
        bgroup' groupTitle (b:bs) = bgroup groupTitle (b : map cmp bs)
          where
            cmp = bcompareWithin 0 1.1 ("$NF == \"List\" && $(NF-1) == \"" ++ groupTitle ++ "\"")

    in
    [ bgroup "Basic interface"

        [ bgroup' "++"  -- should be identical
            [benchL (uncurry ((L.++) :: S -> S -> S) )   
            ,benchS  (uncurry ((S.++) :: S -> S -> S) )  ]
        
        , bgroup' "head"
            [benchL  (L.head :: S -> Char) 
            ,benchS  (S.head :: S -> Char) ]
    
        , bgroup' "last"
            [benchL (L.last :: S -> Char)
            ,benchS (S.last :: S -> Char)
            ]
    
        , bgroup' "init"
            [benchL (L.init :: S -> S)
            ,benchS (S.init :: S -> S)
            ]
    
        , bgroup' "null"
            [benchL (L.null :: S -> Bool)
            ,benchS (S.null :: S -> Bool)
            ]
    
        , bgroup' "length"
            [benchL  (L.length :: S -> Int)
            ,benchS  (S.length :: S -> Int)
            ]
        ]

    , bgroup "List transformations"

        [ bgroup' "map"
            [benchL ( L.map toUpper :: S -> S )
            ,benchS ( S.map toUpper :: S -> S )
            ]
    
        , bgroup' "reverse"
            [ benchL (L.reverse :: S -> S )
            , benchS (S.reverse :: S -> S )
            ]
    
        , bgroup' "intersperse"
            [ benchL (L.intersperse 'x' :: S -> S)
            , benchS (S.intersperse 'x' :: S -> S)
            ]
    
        , bgroup' "intercalate"
            [ benchL (L.intercalate :: S -> [S] -> S) 
            , benchS (S.intercalate :: S -> [S] -> S) 
            ]

    -- transpose is too slow.

    ]

    , bgroup "Reducing lists (folds)"

        [ bgroup' "foldl'"
            [ benchL ( L.foldl' (\a _ -> a+1) 0 :: S -> Int  )
            , benchS ( S.foldl' (\a _ -> a+1) 0  :: S -> Int )
            ]

{-
    , ("foldr",
        [F (     app  ( L.foldr (\_ a -> a+1) 0 :: S -> Int  ) )
        ,F (     app  ( S.foldr (\_ a -> a+1) 0 :: S -> Int ) )
    ])
-}

    ]

    , bgroup "Special folds"

        [ bgroup' "concat"
            [ benchL ((\ss -> L.concat (ss++ss++ss)) :: [S] -> S)
            , benchS ((\ss -> S.concat (ss++ss++ss)) :: [S] -> S)
            ]
    
        , bgroup' "concatMap"
            [ benchL (L.concatMap (\c -> L.replicate 10 c) :: S -> S)
            , benchS (S.concatMap (\c -> S.replicate 10 c) :: S -> S)
            ]
    
        , bgroup' "any"
            [ benchL ( L.any (=='x') :: S -> Bool       )
            , benchS ( S.any (=='x') :: S -> Bool       )
            ]
        , bgroup' "all"
            [ benchL ( L.all (=='x') :: S -> Bool       )
            , benchS ( S.all (=='x') :: S -> Bool       )
            ]
        , bgroup "maximum"
            [{- benchL (L.maximum :: S -> Char)
            ,-} benchS (S.maximum :: S -> Char)
            ]
        , bgroup "minimum"
            [{- benchL (L.minimum :: S -> Char)
            ,-} benchS (S.minimum :: S -> Char)
            ]
        ]

--     -- * Building lists
--     -- ** Scans

    , bgroup "Sublists"

        [ bgroup' "take"
            [ benchL (L.take 100000 :: S -> S) 
            , benchS (S.take 100000 :: S -> S) 
            ]
        , bgroup' "drop"
            [ benchL (L.drop 100000 :: S -> S)
            , benchS (S.drop 100000 :: S -> S)
            ]
    
        , bgroup' "takeWhile"
            [ benchL (L.takeWhile (/='z') :: S -> S)
            , benchS (S.takeWhile (=='z')  :: S -> S)
            ]
        , bgroup' "dropWhile"
            [ benchL (L.dropWhile (/='z')  :: S -> S) 
            , benchS (S.dropWhile (/='z')  :: S -> S) 
            ]
        ]

    , bgroup "Searching"

        [ bgroup' "elem"
            [ benchL (L.elem ('Z') :: S -> Bool) 
            , benchS (S.elem ('Z') :: S -> Bool) 
            ]
        , bgroup' "notElem"
            [ benchL (L.notElem ('Z') :: S -> Bool) 
            , benchS (S.notElem ('Z') :: S -> Bool) 
            ]
    
        -- ** Searching with a predicate
    
        , bgroup' "find"
            [ benchL (L.find (=='Z') :: S -> Maybe Char) 
            , benchS (S.find (=='Z') :: S -> Maybe Char) 
            ]
    
        , bgroup' "filter"
            [ benchL ( L.filter isSpace :: S -> S )
            , benchS ( S.filter isSpace :: S -> S )
            ]
        ]

    , bgroup "Indexing lists"

        [ bgroup' "index"
            [ benchL ((\x -> x L.!! 300000) ::  S -> Char  )
            , benchS ((\x -> x S.!! 300000) ::  S -> Char  )
            ]
    
        , bgroup' "elemIndex"
            [ benchL (L.elemIndex ('Z') :: S -> Maybe Int ) 
            , benchS (S.elemIndex ('Z') :: S -> Maybe Int ) 
            ]
    
        , bgroup' "elemIndices"
            [ benchL (L.elemIndices ('Z') :: S -> [Int] ) 
            , benchS (S.elemIndices ('Z') :: S -> [Int] ) 
            ]
    
        , bgroup' "findIndex"
            [ benchL (L.findIndex (=='Z') :: S -> Maybe Int ) 
            , benchS (S.findIndex (=='Z') :: S -> Maybe Int ) 
            ]
    
        , bgroup' "findIndices"
            [ benchL (L.findIndices (=='Z') :: S -> [Int] ) 
            , benchS (S.findIndices (=='Z') :: S -> [Int] ) 
            ]
        ]

    , bgroup "Zipping and unzipping lists"

        [ bgroup' "zip"
            [ benchL (uncurry (L.zip) :: (S,S) -> [(Char, Char)]     )
            , benchS (uncurry (S.zip) :: (S,S) -> [(Char, Char)]     )
            ]
    
        , bgroup' "zipWith"
            [ benchL (uncurry (L.zipWith (,)) :: (S,S) -> [(Char, Char)]     )
            , benchS (uncurry (S.zipWith (,)) :: (S,S) -> [(Char, Char)]     )
            ]
    
        , bgroup' "replicate"
            [ benchL (const $ L.replicate 2000000 'x' :: S -> S)
            , benchS (const $ S.replicate 2000000 'x' :: S -> S)
            ]
        ]
    ]


{-


    , ("span",
        [F ({-# SCC "span"          #-}     app $ B.span (/=122))
        ,F ({-# SCC "lazy span"     #-}     app $ L.span (/=122))
    ])
    , ("break",
        [F ({-# SCC "break"         #-}     app $ B.break (==122))
        ,F ({-# SCC "lazy break"    #-}     app $ L.break (==122))
    ])
    , ("split",
        [F ({-# SCC "split"         #-}     app $ B.split 0x0a)
        ,F ({-# SCC "lazy split"    #-}     app $ L.split 0x0a)
    ])
--  , ("breakByte",
--      [F ({-# SCC "breakChar"     #-}     app $ B.breakByte 122)
--      ,F ({-# SCC "lazy breakChar" #-}    app $ L.breakByte 122)
--  ])
--  , ("spanByte",
--      [F ({-# SCC "spanChar"      #-}     app $ B.spanByte 122)
--      ,F ({-# SCC "lazy spanChar" #-}     app $ L.spanByte 122)
--  ])

    , ("cons",
        [F ({-# SCC "cons"          #-}     app $ B.cons 120)
        ,F ({-# SCC "lazy cons"     #-}     app $ L.cons 120)
    ])
    , ("snoc",
        [F ({-# SCC "snoc"          #-}     app $ flip B.snoc 120)
        ,F ({-# SCC "lazy snoc"     #-}     app $ flip L.snoc 120)
    ])
    , ("empty",
        [F ({-# SCC "empty"         #-}     const B.empty)
        ,F ({-# SCC "lazy empty"    #-}     const L.empty)
    ])
    , ("head",
        [F ({-# SCC "head"          #-}     app B.head)
        ,F ({-# SCC "lazy head"     #-}     app L.head)
    ])
    , ("tail",
        [F ({-# SCC "tail"          #-}     app B.tail)
        ,F ({-# SCC "lazy tail"     #-}     app L.tail)
    ])

    , ("count",
        [F ({-# SCC "count"         #-}     app $ B.count 10)
        ,F ({-# SCC "lazy count"    #-}     app $ L.count 10)
    ])

    , ("isPrefixOf",
        [F ({-# SCC "isPrefixOf" #-}        app $ B.isPrefixOf
                (C.pack "The Project Gutenberg eBook"))
        ,F ({-# SCC "lazy isPrefixOf" #-}   app $ L.isPrefixOf
                (L.pack [84,104,101,32,80,114,111,106,101
                           ,99,116,32,71,117,116,101,110,98
                           ,101,114,103,32,101,66,111,111,107]))
    ])
    , ("join",
        [F ({-# SCC "join"          #-}     app $ B.join (B.pack [1,2,3]))
        ,F ({-# SCC "lazy join"     #-}     app $ L.join (L.pack [1,2,3]))
    ])
--  , ("joinWithByte",
--      [F ({-# SCC "joinWithByte"  #-}     app $ uncurry (B.joinWithByte 32))
--      ,F ({-# SCC "lazy joinWithByte" #-} app $ uncurry (L.joinWithByte 32))
--  ])

    , ("elem",
        [F ({-# SCC "elem"          #-}     app $ B.elem 122)
        ,F ({-# SCC "lazy elem"     #-}     app $ L.elem 122)
    ])
    , ("notElem",
        [F ({-# SCC "notElem"       #-}     app $ B.notElem 122)
        ,F ({-# SCC "lazy notElem"  #-}     app $ L.notElem 122)
    ])
    , ("elemIndex",
        [F ({-# SCC "elemIndex"     #-}     app $ B.elemIndex 122)
        ,F ({-# SCC "lazy elemIndex" #-}    app $ L.elemIndex 122)
    ])
    , ("findIndices",
        [F ({-# SCC "findIndicies"  #-}     app $ B.findIndices (==122))
        ,F ({-# SCC "lazy findIndices" #-}  app $ L.findIndices (==122))
    ])
    , ("elemIndices",
        [F ({-# SCC "elemIndicies"  #-}     app $ B.elemIndices 122)
        ,F ({-# SCC "lazy elemIndices" #-}  app $ L.elemIndices 122)
    ])
    , ("splitAt",
        [F ({-# SCC "splitAt"       #-}     app $ B.splitAt 10000)
        ,F ({-# SCC "lazy splitAt"  #-}     app $ L.splitAt 10000)
    ])
    , ("splitWith",
        [F ({-# SCC "splitWith"     #-}     app $ B.splitWith (==122))
        ,F ({-# SCC "lazy splitWith" #-}    app $ L.splitWith (==122))
    ])

    , ("group",
        [F ({-# SCC "group"         #-}     app B.group)
        ,F ({-# SCC "lazy group"    #-}     app L.group)
    ])
    , ("groupBy",
        [F ({-# SCC "groupBy"       #-}     app $ B.groupBy (==))
        ,F ({-# SCC "lazy groupBy"  #-}     app $ L.groupBy (==))
    ])
    , ("inits",
        [F ({-# SCC "inits"         #-}     app B.inits)
    ])
    , ("tails",
        [F ({-# SCC "tails"         #-}     app B.tails)
    ])
--  , ("transpose",[F ({-# SCC "transpose" #-}B.transpose [fps,fps'])])

------------------------------------------------------------------------
--
-- Char8 or ByteString only

    , ("intersperse",
        [F ({-# SCC "intersperse"   #-}     app $ B.intersperse 120 )
    ])
    , ("sort",
        [F ({-# SCC "sort"          #-}     app B.sort)
    ])
--  , ("lineIndices",
--      [F ({-# SCC "lineIndicies"  #-}     app C.lineIndices)
--  ])
    , ("elemIndexEnd",
        [F ({-# SCC "elemIndexEnd"  #-}     app $ B.elemIndexEnd 122)
    ])
--  , ("breakSpace",
--      [F ({-# SCC "breakSpace"    #-}     app C.breakSpace)
--  ])
--  , ("dropSpace",
--      [F ({-# SCC "dropSpace"     #-}     app C.dropSpace)
--  ])
--  , ("dropSpaceEnd",
--      [F ({-# SCC "dropSpaceEnd"  #-}     app C.dropSpaceEnd)
--  ])

--  , ("zip",[F ({-# SCC "zip" #-} B.zip fps fps)])

    , ("isSubstringOf",
        [F ({-# SCC "isSubstringOf" #-}     app $ B.isSubstringOf (C.pack "email news"))
    ])
    , ("isSuffixOf",
        [F ({-# SCC "isSuffixOf"    #-}     app $ B.isSuffixOf (C.pack "new eBooks"))
    ])
    , ("spanEnd",
        [F ({-# SCC "spanEnd"       #-}     app $ B.spanEnd (/=122))
    ])
    , ("lines",
        [F ({-# SCC "lines"         #-}     app C.lines)
    ])
    , ("unlines",
        [F ({-# SCC "unlines"       #-}     app C.unlines)
    ])
    , ("words",
        [F ({-# SCC "words"         #-}     app C.words)
    ])
    , ("unwords",
        [F ({-# SCC "unwords"       #-}     app C.unwords)
    ])

 ]
-}

------------------------------------------------------------------------

data Input = Input String String String [String]
instance NFData Input where
    rnf (Input x y z w) = rnf (x,y,z,w)

-- instance Forceable Input where
--   force (Input s x y xs) = force s >> force x >> force y >> force xs

class (Eq a, Ord a) => Ap a where app :: (a -> b) -> Input -> b

instance Ap String            where app f (Input _ x _ _)  = f x
instance Ap [String]          where app f (Input _ _ _ xs) = f xs
instance Ap (String,String)   where app f (Input _ x y _)  = f (x, y)
instance Ap (String,[String]) where app f (Input s _ _ xs) = f (s, xs)

app2 :: Ap (a, b) => (a -> b -> c) -> Input -> c
app2 = app . uncurry

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = L.unfoldr split
  where split [] = Nothing
        split s  = Just (Prelude.splitAt n s)
