{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
module Chapter2.SimpleFunctions ( Client(..),Person(..),Gender(..),TimeMachineR(..) ) where

firstOrEmpty ∷ [String] -> String
firstOrEmpty lst = if not (null lst) then head lst else "empty"

(+++) ::  [a] -> [a] -> [a]
lst1 +++ lst2 = if (null lst1)  {- check for base case -} 
                then lst2  -- base case
                else (head lst1) : (tail lst1 +++ lst2)

(++++) :: [a] -> [a] -> [a]
[] ++++ lst2 = lst2
(x:xs) ++++ lst2 = x : xs ++++ lst2

reverse2 :: [a] -> [a]
reverse2 lst =  if (null lst) 
                then []
                else (reverse2 (tail lst)) +++ [head lst]

maxmin :: (Ord a) => [a] -> (a,a)
maxmin lst = let h = head lst
             in if null (tail lst)
                then (h, h)
                else ( if h > t_max then h else t_max
                    , if h < t_min then h else t_min )
                    where t = maxmin(tail lst)
                          t_max = fst t
                          t_min = snd t

maxmin2 :: (Ord a) => [a] -> (a,a)
maxmin2 [x] = (x,x)
maxmin2 (x:xs) = (if x > xs_max then x else xs_max,
                  if x < xs_min then x else xs_min)
                where (xs_max,xs_min) = maxmin2 xs

data Gender = Male | Female | Unknown
              deriving (Show,Read,Eq,Ord) 

data Client i = GovOrg  { clientId :: i , clientName :: String }
              | Company { clientId :: i , clientName :: String , person :: Person, duty :: String }
              | Individual { clientId :: i , person :: Person }
              deriving (Show,Read,Eq,Ord) 

data Person = Person { firstName :: String, lastName :: String, gender :: Gender }
              deriving (Show,Read,Eq,Ord) 

{-data Client = GovOrg String -}
            {-| Company String Integer Person String-}
            {-| Individual Person Bool-}
            {-deriving Show-}

{-data ClientR = GovOrgR {clientRName :: String}-}
             {-| CompanyR {clientRName :: String, companyId :: Integer, person :: PersonR, duty :: String}-}
             {-| IndividualR {person :: PersonR}-}
             {-deriving Show-}

{-data PersonR = PersonR {firstName :: String, lastName :: String}-}
            {-deriving (Show, Read)-}

getClientName :: Client a -> String 
getClientName client = case client of
                    GovOrg _ name -> name
                    Company _ name _ _ -> name
                    Individual _ person ->
                        case person of Person fName lName _ ->  fName ++ " " ++ lName

data Manufacturer = Manufacturer String
    deriving Show

data TimeTravel = Future | Past
    deriving Show

data Model = Model Integer
    deriving Show

data TimeMachine = TimeMachine Manufacturer Model String TimeTravel Float
    deriving Show

data ManufacturerR = ManufacturerR { manufacturerName :: String}
    deriving Show

data ModelR = ModelR { modelNumber :: Integer }
    deriving Show

data TimeMachineR = TimeMachineR {manufacturer :: ManufacturerR, model :: ModelR, name :: String, timeTravelMode :: TimeTravel, price :: Float}
    deriving Show

genderCount :: [Client a] -> (Integer,Integer)
genderCount [] = (0,0)
genderCount clients = case head clients of
                    Individual _ person -> 
                        case person of 
                            Person _ _ Male -> (1 + fst (genderCount(tail clients)) ,0+ snd (genderCount(tail clients)))
                            Person _ _ Female -> (0 + fst (genderCount(tail clients)),1 + snd (genderCount(tail clients)))
                            _ -> (0 + fst (genderCount (tail clients)),0 + snd (genderCount (tail clients)))
                    _ -> (0 + fst (genderCount (tail clients)),0 + snd (genderCount (tail clients)))

genderCount2 :: [Client a] -> (Integer,Integer)
genderCount2 [] = (0,0)
genderCount2 (x:xs) = case x of
                    Individual _ person -> 
                        case person of 
                            Person _ _ Male -> (1 + fst (genderCount2(xs)) ,0+ snd (genderCount2(xs)))
                            Person _ _ Female -> (0 + fst (genderCount2(xs)),1 + snd (genderCount2(xs)))
                            _ -> (0 + fst (genderCount2 (xs)),0 + snd (genderCount2 (xs)))
                    _ -> (0 + fst (genderCount2 (xs)),0 + snd (genderCount2 (xs)))

applyDiscount :: Float -> TimeMachine -> TimeMachine
applyDiscount discount (TimeMachine manufacturer model name timetravel price) = TimeMachine manufacturer model name timetravel (price * discount)

applyDiscountR :: Float -> TimeMachineR -> TimeMachineR
applyDiscountR discount t@ TimeMachineR {..} = t {price = price * discount}

applyDiscounts :: Float -> [TimeMachine] -> [TimeMachine]
applyDiscounts discount clients = map (applyDiscount discount) clients 

null2 :: [a] -> Bool
null2 [] = True
null2 (x:xs) = False

head2 :: [a] -> a
head2 (x:xs) = x

tail2 :: [a] -> [a]
tail2 (x:xs) = xs
 
sorted2 :: Ord a => [a] -> Bool
sorted2 [] = True
sorted2 [_] = True
sorted2 (x: r@(y:_)) = x < y && sorted2 r
{-sorted2 (x: r@(y:_)) = x < y && sorted2 r-}
{-sorted2 (x:y:xs) = x < y && sorted2 (y:xs)-}


ackermann ::  Integer -> Integer -> Integer
ackermann m n
    | m == 0 = n + 1
    | m > 0 && n == 0 = ackermann (m-1) 1
    | m > 0 && n > 0 = ackermann (m-1) (ackermann m (n - 1))

unzip2 :: [(a,a)] -> ([a],[a])
unzip2 [] = ([],[])
unzip2 (x:xs) = ((fst x : fst (unzip2 xs)), (snd x : snd ( unzip2 xs)))

responsibility :: Client a -> String
responsibility (Company _ _ _ r) = r
responsibility _ = "Unkonwn"

{-view pattern example-}
specialClient :: Client a -> Bool
specialClient (clientName -> "Ronny Gärtner") = True
specialClient (responsibility -> "Director") = True
specialClient _ = False

{-Chapter 3-}
swapTriple :: (a,b,c) -> (b,c,a)
swapTriple (x,y,z) = (y,z,x)

duplicate :: a -> (a,a)
duplicate x = (x,x)

nothing :: a -> Maybe b
nothing _ = Nothing

index :: Num i => [t] -> [(i,t)]
index [] = []
index [x] = [(0,x)]
index (x:xs) = let indexed@((n,_):_) = index xs
                in (n+1,x):indexed

maybeA [] = 'a'

isOne :: Integer -> Bool
isOne i = i == 1

filterOnes :: (Num i,Eq i) => [i] -> [i]
filterOnes numbers = filter (\x -> x == 1) numbers

filterANumber:: (Num i,Eq i) => i -> [i] -> [i]
filterANumber number numbers = filter (\x -> x == number) numbers

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f [] = []
filterNot f (x:xs) 
    | not $ f x = x : (filterNot f xs)
    | otherwise = filterNot f xs

filterGovOrgs :: (Client a -> Bool) -> [Client a] -> [Client a]
filterGovOrgs f [] = []
filterGovOrgs f (x:xs) 
    | f x = x : filterGovOrgs f xs
    | otherwise = filterGovOrgs f xs

isGovOrg :: Client a -> Bool
isGovOrg (GovOrg _ _) =  True
isGovOrg _ = False

filterGovOrgsFunc :: [Client a] -> [Client a]
filterGovOrgsFunc [] = []
filterGovOrgsFunc clients = filterGovOrgs isGovOrg clients 


{-filterGovOrgsLambda :: [Client] -> [Client]-}
{-filterGovOrgsLambda [] = []-}
{-filterGovOrgsLambda clients = filterGovOrgs -}
                                            {-(\x -> case x of -}
                                                    {-GovOrg _ -> True-}
                                                    {-_ -> False) clients -}

filterGovOrgsLambda :: [Client a] -> [Client a]
filterGovOrgsLambda [] = []
filterGovOrgsLambda clients = filterGovOrgs f' clients
                                where 
                                f' (GovOrg _ _) = True
                                f' _ = False
