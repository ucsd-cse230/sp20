
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE TypeSynonymInstances #-}

module Testing where 

import Test.QuickCheck hiding ((===))
import Control.Monad
import Data.List
import qualified Data.Map as M 
import Control.Monad.State hiding (when)

-- >>> incr 5
-- 6
--

incr :: Int -> Int
incr x = x + 1












prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = 
  reverse (xs ++ ys) == reverse xs ++ reverse ys
-- rev ([0] ++ [1]) ==> rev ([0,1]) ==> [1,0]
-- rev [0] ++ rev [1] ==> [0] ++ [1] ==> [0, 1]


-- >>> quickCheck prop_revapp
-- *** Failed! Falsifiable (after 5 tests and 4 shrinks):
-- [0]
-- [1]
--

-- >>> prop_revapp [0] [1]
-- False
--















prop_revapp' :: [Int] -> [Int] -> Bool
prop_revapp' xs ys = 
  reverse (xs ++ ys) == reverse ys ++ reverse xs


-- >>> quickCheck prop_revapp' 
-- +++ OK, passed 100 tests.
--





-- >>> quickCheckN 500 prop_revapp'

quickCheckN n = quickCheckWith (stdArgs { maxSuccess = n } )








qsort        :: (Ord a) => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort ls ++ [x] ++ qsort rs
  where 
    ls       = [y | y <- xs, y < x]  -- elems in xs < x 
    rs       = [z | z <- xs, x < z]  -- elems in xs > x

ls :: [Int]
ls = [1,3..19] ++ [2,4..20]

-- >>> ls
-- [1,3,5,7,9,11,13,15,17,19,2,4,6,8,10,12,14,16,18,20]
--


-- >>> qsort ls
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
--









isOrdered :: (Ord a) => [a] -> Bool
isOrdered (x1:x2:xs) = x1 <= x2 && isOrdered (x2:xs)
isOrdered _          = True

prop_qsort_isOrdered :: [Int] -> Bool
prop_qsort_isOrdered xs = isOrdered (qsort xs)

-- >>> quickCheckN 10000 prop_qsort_isOrdered 
-- +++ OK, passed 10000 tests.
--

-- >>> head [3,1,2,41,2,41,15]
-- 3
--






















prop_qsort_min :: [Int] -> Bool
prop_qsort_min xs = 
  head (qsort xs) == minimum xs

-- >>> quickCheck prop_qsort_min
-- *** Failed! Exception: 'Prelude.head: empty list' (after 1 test):
-- []
--


















prop_qsort_nn_min    :: [Int] -> Property
prop_qsort_nn_min xs =
  not (null xs) ==> head (qsort xs) == minimum xs
  
-- >>> quickCheck prop_qsort_nn_min
-- +++ OK, passed 100 tests.
--
















prop_qsort_sort    :: [Int] -> Bool
prop_qsort_sort xs =  qsort xs == sort xs

-- >>> quickCheck prop_qsort_sort
-- *** Failed! Falsifiable (after 7 tests and 2 shrinks):
-- [3,3]
--

-- >>> qsort [3, 3]  
-- [3]
--

-- >>> noDuplicates [1,2,1]
-- False
--




















noDuplicates ::(Eq a) => [a] -> Bool
noDuplicates (x:xs) = not (x `elem` xs) && noDuplicates xs
noDuplicates _      = True

prop_qsort_distinct :: [Int] -> Bool 
prop_qsort_distinct xs = noDuplicates (qsort xs)  

-- >>> quickCheck prop_qsort_distinct
-- +++ OK, passed 100 tests.
--
















prop_qsort_distinct_sort :: [Int] -> Property 
prop_qsort_distinct_sort xs = 
  (noDuplicates xs) ==> (qsort xs == sort xs)

-- >>> quickCheckN 1000 prop_qsort_sort
-- *** Failed! Falsifiable (after 9 tests and 2 shrinks):
-- [4,4]
--


-- >>> :t choose
-- choose :: (Int, Int) -> Gen Int 
-- >>> :t sample'
-- sample' :: Gen a -> IO [a]












-- >>> sample' pos 
-- [3,1,4,2,7,5,2,10,4,6,1]

pos :: Gen Int
pos = choose (0, 10)

-- xs !! i 
elems :: [a] -> Gen a
elems xs = do 
  i <- choose (0, length xs - 1)
  return (xs !! i)

posPr = do
  x <- pos
  y <- pos
  return (x, y)

-- >>> :t (!!)
-- (!!) :: [a] -> Int -> a
--

-- >>> sample' posPr
-- [(4,7),(10,10),(10,3),(8,8),(9,0),(0,5),(3,2),(1,2),(6,4),(7,1),(0,9)]















posPair = do
  x1 <- pos
  x2 <- pos
  return (x1, x2)

-- >>> sample' posPair








oneOf :: [Gen a] -> Gen a
oneOf gs = do
  g <- elements gs
  x <- g
  return x

-- >>> sample' (oneOf [choose (0,2), choose (10,12)])















randomThings :: (Arbitrary a) => IO [a]
randomThings = sample' arbitrary

-- >>> randomThings :: IO [Int]
-- [0,2,4,1,-8,-7,-2,6,6,1,-2]
--
-- >>> randomThings :: IO [Bool]
-- [False,False,True,True,True,False,True,False,False,False,False]
--

-- >>> randomThings :: IO [String]
-- ["","","W=","\126284\f7","","ir\b\980235m","b\"\FS7#>r\205817z","p_\644248\987639\EOTj\EM\ESCke4\371871\ESC>","{^@\DEL",")E","'\n\133081u\r"]
--


-- >>> randomThings :: IO [(Int, Bool)] 
-- [(0,True),(1,True),(0,True),(6,False),(-5,True),(4,False),(-12,False),(-8,False),(5,False),(-9,False),(-7,False)]
--


















data Variable 
  = V String 
  deriving (Eq, Ord)

data Value 
  = IntVal Int
  | BoolVal Bool
  deriving (Eq, Ord)

data Expression 
  = Var   Variable
  | Val   Value
  | Plus  Expression Expression
  | Minus Expression Expression

data Statement
  = Assign   Variable   Expression
  | If       Expression Statement  Statement
  | While    Expression Statement
  | Sequence Statement  Statement
  | Skip

type WState = M.Map Variable Value

instance Arbitrary Variable where
  arbitrary = do
    x <- elements ['A'..'Z'] 
    return (V [x])

-- >>> randomThings :: IO [Variable]
-- [V "X",V "V",V "W",V "C",V "J",V "G",V "H",V "I",V "D",V "T",V "R"]
--

instance Arbitrary Value where
  arbitrary = oneOf 
    [ do {n <- arbitrary; return (IntVal n) }
    , do {b <- arbitrary; return (BoolVal b)} 
    ]

instance Arbitrary Expression where
  arbitrary = expr

expr :: Gen Expression
expr     = oneof [baseE, binE] 

binE :: Gen Expression
binE  = do 
  o  <- elements [Plus, Minus]
  e1 <- expr
  e2 <- expr 
  return (o e1 e2)

baseE :: Gen Expression
baseE = oneOf 
  [ do {x <- arbitrary; return (Var x) }
  , do {v <- arbitrary; return (Val v)} 
  ]


-- >>> randomThings :: IO [WState]
-- [fromList [],fromList [(V "P",IntVal 0)],fromList [(V "M",IntVal 0),(V "Z",IntVal 1)],fromList [(V "E",BoolVal False),(V "J",BoolVal False),(V "X",IntVal 6)],fromList [(V "J",IntVal (-8)),(V "U",IntVal 3),(V "Z",BoolVal True)],fromList [(V "A",BoolVal False),(V "I",BoolVal False),(V "J",BoolVal False)],fromList [(V "H",BoolVal True),(V "J",IntVal (-9)),(V "K",IntVal (-12)),(V "L",BoolVal True),(V "U",BoolVal False),(V "V",IntVal (-4)),(V "W",IntVal (-9))],fromList [(V "A",BoolVal True),(V "C",BoolVal False),(V "E",IntVal 1),(V "K",BoolVal False),(V "N",IntVal (-7)),(V "P",BoolVal True),(V "R",IntVal (-2)),(V "T",BoolVal True),(V "V",IntVal (-1)),(V "Z",BoolVal True)],fromList [(V "A",BoolVal True),(V "M",BoolVal True),(V "O",BoolVal True),(V "S",IntVal 14),(V "W",IntVal 3)],fromList [(V "D",BoolVal True),(V "E",IntVal (-5)),(V "F",BoolVal True),(V "L",BoolVal False),(V "M",IntVal (-13)),(V "T",BoolVal True),(V "V",IntVal (-3)),(V "Z",BoolVal True)],fromList [(V "D",IntVal 13),(V "F",IntVal 16),(V "I",IntVal (-14)),(V "M",IntVal 11),(V "O",BoolVal True),(V "P",BoolVal False),(V "Q",BoolVal False),(V "R",BoolVal False),(V "S",BoolVal False),(V "U",BoolVal True),(V "Y",IntVal 15)]]
--

execute ::  WState -> Statement -> WState
execute s0 stmt = execState (evalS stmt) s0

(===) ::  Statement -> Statement -> Property
p1 === p2 = forAll arbitrary (\st -> execute st p1 == execute st p2)


-- X := 10; Y := 20
prog1 = Sequence 
  (Assign (V "X") (Val (IntVal 10)))
  (Assign (V "Y") (Val (IntVal 20)))

--  Y := 20; X := 10
prog2 = Sequence 
  (Assign (V "Y") (Val (IntVal 20)))
  (Assign (V "X") (Val (IntVal 10)))

--  Y := 20; X := 20
prog3 = Sequence 
  (Assign (V "Y") (Val (IntVal 20)))
  (Assign (V "X") (Val (IntVal 20)))

-- >>> quickCheck (prog1 === prog2)

-- >>> quickCheck (prog1 === prog3)

prop_add_zero_elim :: Variable -> Expression -> Property
prop_add_zero_elim x e = 
   (x `Assign` (e `Plus` Val (IntVal 0))) === (x `Assign` e) 

prop_sub_zero_elim :: Variable -> Expression -> Property
prop_sub_zero_elim x e =
  (x `Assign` (e `Minus` Val (IntVal 0))) === (x `Assign` e)

-- B := TRUE 
-- B := TRUE + 0  

-- >>> quickCheck prop_add_zero_elim
-- *** Failed! Falsifiable (after 1 test):
-- B
-- True
-- fromList []
--

p1  = (V "W") `Assign` (Val (BoolVal True))
p2  = (V "W") `Assign` ((Val (BoolVal True) `Plus` Val (IntVal 0)))
st0 = M.fromList []

-- >>> execute st0 p1
-- fromList [(W,True)]
--

-- >>> execute st0 p2
-- fromList [(W,0)]
--



-- >>> :i Applicative
-- class Functor f => Applicative (f :: * -> *) where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b


-- >>> (\x -> x + 10) <$> [1..10] 
-- [11,12,13,14,15,16,17,18,19,20]
--

-- instance Applicative (Either e) -- Defined in ‘Data.Either’
-- instance [safe] (Functor m, Monad m) => Applicative (StateT s m)
--   -- Defined in ‘transformers-0.5.5.0:Control.Monad.Trans.State.Lazy’
-- instance [safe] Applicative Gen -- Defined in ‘Test.QuickCheck.Gen’
-- instance Applicative [] -- Defined in ‘GHC.Base’
-- instance Applicative Maybe -- Defined in ‘GHC.Base’
-- instance Applicative IO -- Defined in ‘GHC.Base’
-- instance Applicative ((->) a) -- Defined in ‘GHC.Base’
-- instance Monoid a => Applicative ((,) a) -- Defined in ‘GHC.Base’
--


exprI :: Gen Expression
exprI = oneof [baseI, binE] 

baseI :: Gen Expression
baseI = oneOf 
  [ do {x <- arbitrary; return (Var x) }
  , do {n <- arbitrary; return (Val (IntVal n)) } 
  ]

binI :: Gen Expression
binI  = do 
  o  <- elements [Plus, Minus]
  e1 <- exprI
  e2 <- exprI 
  return (o e1 e2)

prop_add_zero_elim'   :: Variable -> Property
prop_add_zero_elim' x = 
  forAll exprI (\e -> (x `Assign` (e `Plus` Val (IntVal 0))) === (x `Assign` e))


-- >>> quickCheck prop_add_zero_elim'


prop_const_prop :: Variable -> Variable -> Expression -> Property
prop_const_prop x y e = 
  ((x `Assign` e) `Sequence` (y `Assign` e))
  ===
  ((x `Assign` e) `Sequence` (y `Assign` Var x))


-- >>> quickCheck prop_const_prop 







-------------------------------------------------------------------------------------------
-- HINT FOR HW
-------------------------------------------------------------------------------------------



data BST k v 
  = Node k v (BST k v) (BST k v) 
  | Leaf
  deriving (Show)


genBST :: Int -> Int -> Gen (BST Int String)

genBST _ 0 = 
  return Leaf

genBST lo h = do
  l <- genBST lo (h-1)
  let lo' = maxKey l
  k <- choose (lo', lo' + 10)
  r <- genBST (lo' + 11) (h-1)
  v <- arbitrary
  return (Node k v l r)

maxKey :: BST Int a -> Int
maxKey = error "fill this in"

-- >>> sample' (genBST 3)
-- [Node 0 "" (Node 0 "" (Node 0 "" Leaf Leaf) (Node 0 "" Leaf Leaf)) (Node 0 "" (Node 0 "" Leaf Leaf) (Node 0 "" Leaf Leaf)),Node 0 "\274834" (Node 2 "'" (Node (-2) "nW" Leaf Leaf) (Node (-1) "\62507\EM" Leaf Leaf)) (Node 1 "" (Node (-2) "^\374899" Leaf Leaf) (Node 2 "5" Leaf Leaf)),Node 2 "a\590219\SO" (Node (-1) "\444168\&7" (Node 2 "\\\RS\1106722\CAN" Leaf Leaf) (Node 4 "" Leaf Leaf)) (Node (-1) "" (Node (-1) "" Leaf Leaf) (Node 0 "" Leaf Leaf)),Node 5 "\1062981\ETB\437131I@c" (Node 1 "d\133939>" (Node 4 "Uc\635778)\218651\106777" Leaf Leaf) (Node 6 "Q5\ETB" Leaf Leaf)) (Node 2 "*9" (Node 4 "\107985" Leaf Leaf) (Node (-6) "JGh" Leaf Leaf)),Node 6 "\EM:N\GS,\790863\142233" (Node (-6) "E[\r" (Node 8 "n\STX\1017783*T/" Leaf Leaf) (Node 2 "0\DC2b:|tA" Leaf Leaf)) (Node (-3) "\ENQZ+m" (Node (-4) "XEa\SUB\DLE)\564673" Leaf Leaf) (Node 2 "o1\NAK\r\136958\849695I>" Leaf Leaf)),Node (-5) "\471985K\rB\191348\EMZW" (Node 7 "t\US@[X7~" (Node 2 ",\SYN2g\DC2\475153\351146\&7jL" Leaf Leaf) (Node 10 "\68993K\SYN\GSz\CAN\RSF\\k" Leaf Leaf)) (Node 1 "#}+\514269\&5" (Node 8 "Pj\902881\727307\SI\215773\711909\955857?" Leaf Leaf) (Node (-3) ")m\1020007\SI\ACK" Leaf Leaf)),Node (-7) "" (Node (-2) "'!M\759215\778838\619394J0" (Node (-10) "B5\1078238_\ESC!" Leaf Leaf) (Node 5 "6" Leaf Leaf)) (Node 7 "4Jc\SOHV\DLE" (Node 0 "qJ\530817EAw\239564Sa\507201h1" Leaf Leaf) (Node 7 ">ZX\SUBC\183141" Leaf Leaf)),Node (-8) "\49444" (Node 3 "\DEL\tFK\953920\a\960301\ETXE\765654\98879\&8\423231Y" (Node 4 "&L/\t)y\SYN\DC1\905023&\NUL" Leaf Leaf) (Node 8 "\471718\DC1Mh\SI_O\405238" Leaf Leaf)) (Node 3 "`\DEL;" (Node (-12) "\45260\SOHT\374131" Leaf Leaf) (Node 12 "/\991698|\357702cO\480823\EOT\684912E5w:\798568" Leaf Leaf)),Node 0 "t5f\SO\797788C;\237869/\"!" (Node 13 "\629259" (Node 11 "(\SUB\v\93969" Leaf Leaf) (Node 10 "\SO" Leaf Leaf)) (Node (-10) "_^\SI\161183\&7^-!\589359\ETXY\GS\886557" (Node 6 "" Leaf Leaf) (Node (-9) "\SOH;qeC" Leaf Leaf)),Node (-2) "\v\360020 \ESC\66175\&2W~\887060l" (Node 8 "U\711822-" (Node 11 "\f'\SIk\38198\507079\708045.\SI" Leaf Leaf) (Node 2 "Z\1076051\DC1ccbs\799450N\"|JO" Leaf Leaf)) (Node (-3) "" (Node 12 "P\545112\US\406679\116661D\113391n/b^\842399\615486" Leaf Leaf) (Node 8 "0\a\312891\782336cE]mz\142393\r\87913" Leaf Leaf)),Node (-3) "\694150" (Node (-9) "\863183x=1v#\162221\SOH\DC2L" (Node (-9) "]\SO%\a9fNBI\182820.\335657\975749\285406[sFA\CANy" Leaf Leaf) (Node (-18) "\SOH\1016680\1047755j.\CAN\DLE\ESC" Leaf Leaf)) (Node (-17) "=R\CAN\86043\"\61476\294881\173364f\902302n" (Node (-16) "!&H#8w" Leaf Leaf) (Node 19 "9" Leaf Leaf))]
--









genBal :: Int -> Int -> Int -> Gen (BST Int String)
genBal 0 lo hi = 
  return Leaf
genBal n lo hi = do
  key   <- choose (lo, hi)
  val   <- arbitrary
  left  <- genBal (n-1) lo (key - 1)
  right <- genBal (n-1) (key + 1) hi
  return (Node key val left right)


-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------


evalE :: Expression -> State WState Value
evalE (Var x)       = get >>= return . M.findWithDefault (IntVal 0) x
evalE (Val v)       = return v
evalE (Plus e1 e2)  = return (intOp (+) 0 IntVal) `ap` evalE e1 `ap` evalE e2
evalE (Minus e1 e2) = return (intOp (-) 0 IntVal) `ap` evalE e1 `ap` evalE e2

evalS :: Statement -> State WState ()
evalS w@(While e s)    = evalS (If e (Sequence s w) Skip)
evalS Skip             = return ()
evalS (Sequence s1 s2) = evalS s1 >> evalS s2
evalS (Assign x e )    = do v <- evalE e
                            m <- get
                            put $ M.insert x v m
                            return ()
evalS (If e s1 s2)     = do v <- evalE e
                            case v of 
                              BoolVal True  -> evalS s1
                              BoolVal False -> evalS s2
                              _             -> return ()


intOp :: (Int -> Int -> a) -> a -> (a -> Value) -> Value -> Value -> Value
intOp op _ c (IntVal x) (IntVal y) = c $ x `op` y
intOp _  d c _          _          = c d 


blank   :: Int -> String 
blank n = replicate n ' '

instance Show Variable where
  show (V x) = x

instance Show Value where
  show (IntVal  i) = show i
  show (BoolVal b) = show b

instance Show Expression where
  show (Var v)       = show v
  show (Val v)       = show v
  show (Plus e1 e2)  = show e1 ++ " + " ++ show e2
  show (Minus e1 e2) = show e1 ++ " + " ++ show e2

instance Show Statement where
  show = showi 0

showi :: Int -> Statement -> String 
showi n (Skip)       = blank n ++ "skip"
showi n (Assign x e) = blank n ++ show x ++ " := " ++ show e
showi n (If e s1 s2) = blank n ++ "if " ++ show e ++ " then\n" ++ 
                       showi (n+2) s1 ++
                       blank n ++ "else\n" ++ showi (n+2) s2 ++ blank n ++ "endif"

showi n (While e s)  = blank n ++ "while " ++ show e ++ " do\n" ++ 
                       showi (n+2) s
showi n (Sequence s1 s2) = showi n s1 ++ "\n" ++ showi n s2 

instance Arbitrary Statement where
  arbitrary = oneof [ liftM2 Assign   arbitrary arbitrary
                    , liftM3 If       arbitrary arbitrary arbitrary
                    , liftM2 While    arbitrary arbitrary
              , liftM2 Sequence arbitrary arbitrary
                    , return Skip ]

