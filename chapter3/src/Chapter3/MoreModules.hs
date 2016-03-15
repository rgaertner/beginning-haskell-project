{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE RecordWildCards #-}
module Chapter3.MoreModules ( listOfClients,compareClient,companyAnalytics,companyDutiesAnalytics) where

import Data.Function (on)
import Data.List (permutations, find, sortBy, groupBy)
import Data.Maybe (isJust)
import Chapter2.SimpleFunctions (Client(..),Person(..),Gender(..),TimeMachineR(..) )
import GHC.Exts

permutationsStartingWith :: Char -> String -> [String]
permutationsStartingWith letter = filter (\l -> head l == letter) . permutations 

elem' :: Eq a => a -> [a] -> Bool
elem' y (xs) = isJust $ find (==y) xs

compareClient :: Client a -> Client a -> Ordering
compareClient (Individual{person = p1}) (Individual{person = p2})
                                = compare (firstName p1) (firstName p2)
compareClient (Individual {}) _ = GT
compareClient _ (Individual {}) = LT
compareClient c1 c2             = compare (clientName c1) (clientName c2)

listOfClients = [ Individual 2 (Person "H. G." "Wells" Male)
                , GovOrg 3 "NTTF"  -- National Time Travel Foundation
                , Company 4 "Wormhole Inc." (Person "Karl" "Schwarzschild" Male) "Physicist"
                , Individual 5 (Person "Doctor" "" Male)
                , Individual 6 (Person "Sarah" "Jane" Female)
                ]

companyDutiesAnalytics :: [Client a] -> [String]
companyDutiesAnalytics = map (duty . head) .
                        sortBy ( flip (compare `on` length)) .
                        groupBy ((==) `on` duty ) .
                        filter isCompany
                    where isCompany (Company {}) = True
                          isCompany _             = False

companyAnalytics :: [Client a] -> [(String, [(Person, String)])]
companyAnalytics clients =  [(the clientName, zip person duty)
                    | client@(Company {..}) <- clients
                    , then sortWith by duty
                    , then group by clientName using groupWith
                    , then sortWith by length client
                    ]

enum :: Int -> Int -> [Int]
enum a b | a > b = []
enum a b = a : enum (a+1) b

withPositions :: [a] -> [(Int,a)]
withPositions list = zip (enum 1 $ length list) list

doubleOdds :: [Int] -> [Int]
doubleOdds list = map (*2) $ filter odd list

doubleOdds2 :: [Int] -> [Int]
doubleOdds2 list = [2 * x | x <- list, odd x]
