import System.IO
import Data.List 
import Control.Monad

{-|
        NOT-SO-RANDOM NOTES:
        -> to this moment, I'm not considering the existence of isolated nodes
        that is, I'm taking as {V u E} the elements of the rho file.
        
        -> this is a simpler, more 'contents-of-the-files-like' version of my initial idea
        this is because the the initial, more Object-Oriented, version is not only more difficult to implement
        but also has a bigger overhead when creating (populating) the PG.
        
        -> by requirement, populate has to use IO, 
        but this violates the principle of aggregating the IO action into the main function
        also this forces us to return IO PG
        
        
-}

-------------------------------
---------TYPE SYNONYMS---------
-------------------------------

type Label = String

type Property = String

type Identificator = String

type Node = Identificator

type Edge =  Identificator

type PropertyAndValue = (Identificator,Property,Val)

type ElementAndLabel = (Identificator,Label)

type PG = ([Node],[Edge],[ElementAndLabel],[PropertyAndValue])

-------------------------------
-------DATA DECLARATIONS-------
-------------------------------

data Val = IntValue Int | DecValue Double | Text String | Binary Bool -- | Date

--data ElementAndLabel = (Node,Label) | (Edge,Label)

-------------------------------
-----INSTANCE DECLARATIONS-----
-------------------------------

--instance Show PGEntry where
--        show (NodeEntry node label _) = " Node: " ++ node ++ " label: " ++ label
--        show (EdgeEntry (n1,n2) label _) = " Edge going from " ++ n1 ++ " to " ++ n2 ++ " with label " ++ label

instance Show Val where
        show (IntValue int) = show int
        show (DecValue double) = show double
        show (Text text) = show text
        show (Binary bool) = show bool
        
-------------------------------
------AUXILIARY FUNCTIONS------
-------------------------------

setify :: [String] -> [String]

setify = map head . group . sort

--

getTreatedVandE :: [[String]] -> ([Node],[Edge])

getTreatedVandE rhoLines = ([node | [_,node,_] <- rhoLines] ++ [node | [_,_,node] <- rhoLines],
                           [edge | [edge,_,_] <- rhoLines])

--

getLambdaContents :: [[String]] -> [ElementAndLabel]

getLambdaContents lambdaLines = [(element,label) | [element,label] <- lambdaLines]

--

getVal :: String -> String -> Val

--getVal valtype value = Text value
getVal "Int" value = IntValue (read value :: Int)
getVal "Double" value = DecValue (read value :: Double)
getVal "Bool" value = Binary (read value :: Bool)
getVal _ value = Text value

--

getValType :: [[String]] -> String -> String

getValType [] _ = "error"
getValType ([propname,proptype]:rest) property
        | propname == property = proptype
        | otherwise = getValType rest property
--

getPropAndValues :: [[String]] -> [[String]] -> [PropertyAndValue]

getPropAndValues sigma prop = [(identificator,property,(getVal valtype value)) 
                                | [identificator,property,value] <- sigma ,
                                let valtype = getValType prop property]


-------------------------------
--------MAIN FUNCTIONS---------
-------------------------------

populate :: String -> String -> String -> String -> IO PG--String -> String -> String -> String -> PG

populate rhofilename lambdafilename sigmafilename propfilename  = do
                                rhocontents <- readFile rhofilename
                                let rhoInputLines = lines rhocontents
                                let (nodes,edges) = getTreatedVandE $ map words rhoInputLines
                                let nodesSet = setify nodes

                                lambdacontents <- readFile lambdafilename
                                let lambdaLines = lines lambdacontents
                                let lambda = getLambdaContents $ map words lambdaLines
                                
                                sigmacontents <- readFile sigmafilename
                                propcontents <- readFile propfilename
                                let sigmalines = lines sigmacontents
                                let proplines = lines propcontents
                                let propertiesAndValues = getPropAndValues (map words sigmalines) (map words proplines)
                                --print(nodesSet)
                                --print(edges)
                                --print(lambda)
                                --print(propertiesAndValues)
                                return (nodesSet, edges, lambda, propertiesAndValues)
                                
