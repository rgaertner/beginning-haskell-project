{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE RecordWildCards #-}
module Chapter4.Container where

import Chapter2.SimpleFunctions (Client(..),Person(..),Gender(..),TimeMachineR(..) )
import Chapter3.MoreModules (listOfClients,compareClient,companyAnalytics,companyDutiesAnalytics)
import qualified Data.Map as M
import qualified Data.Set as S

insert' :: Ord k => k -> a -> M.Map k a -> M.Map k a 
insert' k v m = M.alter (\Nothing -> Just(v)) k m 

adjust' :: Ord k => (a -> a) -> k -> M.Map k a -> M.Map k a
adjust' t k m = M.alter(\(Just(v)) -> Just(t v)) k m

delete' :: Ord k => k -> M.Map k a -> M.Map k a
delete' k m = M.alter (\(Just(v)) -> Nothing) k m


data ClientKind = GovOrgKind | CompanyKind | IndividualKind
            deriving (Show,Read,Eq,Ord)

classifyClients :: [Client Integer] -> M.Map ClientKind (S.Set(Client Integer))
classifyClients [] = M.fromList [(GovOrgKind,S.empty),(CompanyKind,S.empty),(IndividualKind,S.empty)]
classifyClients (c:cs) = M.adjust (S.insert(c)) (getKind c) $ classifyClients cs 
                     where getKind (Company {}) = CompanyKind
                           getKind (GovOrg {}) = GovOrgKind
                           getKind (Individual {}) = IndividualKind

class Nameable n where
  naming :: n -> String
  initial :: Nameable n => n -> Char
  initial n = head (naming n)

instance Nameable (Client a ) where
  naming Individual { person = Person {firstName = f, lastName = n}} = f ++ " " ++ n
  naming c = clientName c

class Priceable p where
   pricing :: p -> Float
   totalPrice :: Priceable p => [p] -> Float
   totalPrice [] = 0
   totalPrice (x:xs) = pricing x + totalPrice (xs)

instance Priceable (TimeMachineR) where
  pricing t = price t 
