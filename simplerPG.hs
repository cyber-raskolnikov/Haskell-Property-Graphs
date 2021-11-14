import System.IO
import Data.List 
import Control.Monad
import Control.Concurrent
import Data.Maybe

{-|
        NOT-SO-RANDOM NOTES:
        ->  the existence of isolated nodes is not considered,
        that is, I'm taking as {V u E} the elements of the rho file.
        
        -> by requirement, populate has to use IO, 
        this forces us to return IO PG in populate, slightly deviating from the specification demands
        
        -> overall, some slight deviations from the practice task demands have been implemented,
        as the specification is somewhat loose and teachers have pointed out it should be treated
        as a non-strict guideline
        
        -> PENDING JP RESPONSE --> CHANGE DOC ON THIS AND QUERY FUNCTIONSabout returning the Val type on the query functions, it would have been possible to do so and print it
        in a proper way, initially I thought about doing so, but the task specifically demands this not to be done.
        Instead, the Val type contents will be returned as Strings at those functions.
        
        -> the Date type is treated as a String
        
        -> a (very small) concession has been taken on the 120-characters long code lines so I could add some ASCII art
        
-}

-------------------------------
---------TYPE SYNONYMS---------
-------------------------------

type Label = String

type Property = String

type Identifier = String -- this type definition is useful, as it allows to refer to both nodes and edges

type Node = Identifier

type Edge =  (Identifier,Node,Node)

type PropertyAndValue = (Identifier,Property,Val)

type ElementAndLabel = (Identifier,Label)

type PG = ([Node],[Edge],[ElementAndLabel],[PropertyAndValue])

-------------------------------
-------DATA DECLARATIONS-------
-------------------------------

data Val = IntValue Int | DecValue Double | Text String | Binary Bool -- | Date

-------------------------------
-----INSTANCE DECLARATIONS-----
-------------------------------

instance Show Val where
        show (IntValue int) = show int
        show (DecValue double) = show double
        show (Text text) = show text
        show (Binary bool) = show bool
        
-------------------------------
------AUXILIARY FUNCTIONS------
-------------------------------

setify :: [String] -> [String]

-- given a list of strings
-- returns the list ordered and without repeated elements

setify = map head . group . sort

--

getTreatedVandE :: [[String]] -> ([Node],[Edge])

-- given the contents of the rho file lines separated by words
-- returns the list of nodes and the list of edges defined in it

getTreatedVandE rhoLines = ([node | [_,node,_] <- rhoLines] ++ [node | [_,_,node] <- rhoLines],
                           [(edge,n1,n2) | [edge,n1,n2] <- rhoLines])

--

getLambdaContents :: [[String]] -> [ElementAndLabel]

-- given the contents of the lambda file lines separated by words
-- returns the list of tuples (element and its label)

getLambdaContents lambdaLines = [(element,label) | [element,label] <- lambdaLines]

--

getVal :: String -> String -> Val

-- given a read value and its read type
-- returns the value wrapped in the corresponding type

getVal "Int" value = IntValue (read value :: Int)
getVal "Double" value = DecValue (read value :: Double)
getVal "Bool" value = Binary (read value :: Bool)
getVal "Date" value = Text value        -- DATE CASE
getVal _ value = Text value

--

dictLookup :: [[String]] -> String -> String

-- given a list of lists of 2 strings a string S to search
-- returns the second element of the list whose first element is S

dictLookup [] _ = ""
dictLookup ([propname,proptype]:rest) property
        | propname == property = proptype
        | otherwise = dictLookup rest property
--

getPropAndValues :: [[String]] -> [[String]] -> [PropertyAndValue]

-- given the contents of the sigma and prop file
-- returns a list of tuples (property name, correctly typed value)

getPropAndValues sigma prop = [(identificator,property,(getVal valtype value)) 
                                | [identificator,property,value] <- sigma ,
                                let valtype = dictLookup prop property]
                                
--

labelLookup :: Identifier -> [ElementAndLabel] -> Label

-- given an identificator and the list that maps ids and labels
-- returns the corresponding label to the identificator

labelLookup _ [] = "\0"
labelLookup id ((currId,currLab):rest)
        | id == currId = currLab
        | otherwise = labelLookup id rest
        
--
        
propsLookup :: Identifier -> [PropertyAndValue] -> [(Property,Val)]

-- given an indentificator and the list that maps ids to their properties & values
-- returns the list of tuples (property,value) corresponding to the given id

propsLookup _ [] = []
propsLookup id ((currId,currProp,currVal):rest)
                | id == currId = (currProp,currVal) : (propsLookup id rest)
                | otherwise = (propsLookup id rest)

--

getNodeInfo :: [ElementAndLabel] -> [PropertyAndValue] -> Node -> (Node,Label,[(Property,Val)])

-- given the functions lambda and sigma and a node
-- returns the complete information contained about that node

getNodeInfo labels properties node = (node,nodeLabel,nodeProps)
                                where 
                                nodeLabel = labelLookup node labels
                                nodeProps = propsLookup node properties
                                
-- 
                               
getEdgeInfo :: [ElementAndLabel] -> [PropertyAndValue] -> Edge -> (Edge,Label,[(Property,Val)])

-- given the functions lambda and sigma and an edge
-- returns the complete information contained about that edge

getEdgeInfo labels properties (id,n1,n2) = ((id,n1,n2),edgeLabel,edgeProps)
                                where
                                edgeLabel = labelLookup id labels
                                edgeProps = propsLookup id properties
                                
--
                                
getStrProps :: [(Property,Val)] -> String

-- given a list of tuples (property,value)
-- returns the contents of the list in a string properly formatted

getStrProps [] = ""
getStrProps ((p,v):rest) = "(" ++ (id p) ++ "," ++ (show v) ++ ")" ++ (getStrProps rest)

--
                                
showNodeInfo :: (Node,Label,[(Property,Val)]) -> IO ()

-- given all the properly formatted information about a node
-- prints it in the correct format specified by the practice task

showNodeInfo (node,label,props) = do
                                let strProps = getStrProps props
                                let line = node ++ " [" ++ label ++ "] {" ++ strProps ++ "}"
                                putStrLn (id line) 
                                
--

showEdgeInfo :: (Edge,Label,[(Property,Val)]) -> IO ()

-- given all the properly formatted information about an edge
-- prints it in the correct format specified by the practice task

showEdgeInfo ((name,n1,n2),label,props) = do
                                        let strProps = getStrProps props
                                        let line = "(" ++ n1 ++ ")" ++ " -- " ++ name ++ "[" ++ label ++ "] --> " ++
                                                   "(" ++ n2 ++ ") {" ++ strProps ++ "}"
                                        putStrLn (id line)
                                        
--

defProp :: PG -> PropertyAndValue -> PG

-- given a property graph and an indentificator of an existing node/edge, an existing property and a valid value
-- returns a property graph whose value for that property for that edge or node is value

defProp (nodes,edges,labels,prop) newPropVal = (nodes,edges,labels,newProp)
                                                where newProp = prop ++ [newPropVal]

--

defLabel :: PG -> ElementAndLabel -> Maybe PG

defLabel (nodes,edges,labels,prop) (id,label)
                          | labelLookup id labels == "\0" = Just (nodes,edges,labels ++ [(id,label)],prop)
                          | otherwise = Nothing
                                                
-------------------------------
---------PG FUNCTIONS----------
-------------------------------

populate :: String -> String -> String -> String -> IO PG

-- given 4 properly formatted rho, lambda, sigma and prop filenames
-- it returns an IO Property Graph containing the information codified in the files

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

                                return (nodesSet, edges, lambda, propertiesAndValues)
                                
--
                                
addEdge :: PG -> Identifier -> Node -> Node -> PG

-- given a PG, an unused edge id. and existing (or not) nodes n1,n2 where the edge n1->n2 does not exist
-- it returns the original PG with a new edge named id. from the first node to the second

addEdge (nodes,edges,l,p) id n1 n2 = (nodesPlus,edgesPlus,l,p)
                                where edgesPlus = edges ++ [(id,n1,n2)]
                                      nodesPlus = setify (nodes ++ [n1,n2])
                                
--

showGraph :: PG -> IO ()

-- given a PG,
-- prints its contents formatted as requested by the practice task

showGraph (nodes,edges,labels,properties) = do
                                putStrLn ""

                                let nodesInfo = map (getNodeInfo labels properties) nodes
                                mapM showNodeInfo nodesInfo     -- MONAD MAP
                                
                                let edgesInfo = map (getEdgeInfo labels properties) edges
                                mapM showEdgeInfo edgesInfo     -- MONAD MAP
                                
                                putStrLn ""                          
--
                                
defVprop :: PG -> Node -> (Property,Val) -> PG

-- given a property graph, an existing node and property, and a valid value
-- it returns a property graph whose value for that property for the node is value

defVprop pg id (prop,val) = defProp pg (id,prop,val)
                                
--

defEprop :: PG -> Edge -> (Property,Val) -> PG

-- given a property graph, an existing edge and property, and a valid value
-- it returns a property graph whose value for that property for the edge is value

defEprop pg (id,_,_) (prop,val) = defProp pg (id,prop,val)
                                
--

defVlabel :: PG -> Node -> Label -> Maybe PG

-- given a property graph, an existing node and a label 
-- it returns a property graph with said node labeled OR Nothing if the node was already labeled 

defVlabel pg id label = defLabel pg (id,label)

--

defElabel :: PG -> Edge -> Label -> Maybe PG

-- given a property graph, an existing edge and a label 
-- it returns a property graph with said edge labeled OR Nothing if the edge was already labeled 

defElabel pg (id,_,_) label = defLabel pg (id,label)
                                
-------------------------------
--------QUERY FUNCTIONS--------
-------------------------------

sigmaPrima :: PG -> Identifier -> [(Property,Val)]

-- given a property graph and an identifier
-- it returns the set of properties and values related to the identifier
-- with the values represented as strings

sigmaPrima (_,_,_,properties) id = stringedPropsLookup
                              where stringedPropsLookup = (propsLookup id properties)
                              
--

propV :: PG -> Int -> Property -> [(Label,Val)]

-- given a property graph, a natural number and a property
-- returns the first k (or less if not existing) pairs (label(v),value)
-- where v is a node having the said property defined, and value is that node's said property value

propV (nodes,edges,labels,properties) k wantedProp = take k listLabelVal
        where listLabelVal = [(labelLookup node labels,valueProp) | node <- nodes,
                                             (prop,valueProp) <- (sigmaPrima (nodes,edges,labels,properties) node),
                                             wantedProp == prop]

--

propE :: PG -> Int -> Property -> [(Label,Val)]

-- given a property graph, a natural number and a property
-- returns the first k (or less if not existing) pairs (label(e),value)
-- where e is an edge having the said property defined, and value is that edge's said property value

propE (nodes,edges,labels,properties) k wantedProp = take k listLabelVal
        where listLabelVal = [(labelLookup edgeID labels,valueProp) | (edgeID,_,_) <- edges,
                                             (prop,valueProp) <- (sigmaPrima (nodes,edges,labels,properties) edgeID),
                                             wantedProp == prop]

-------------------------------
---------MAIN FUNCTION---------
-------------------------------

main :: IO ()

main = do
        putStrLn ""
        putStrLn "██████╗░██████╗░░█████╗░██████╗░███████╗██████╗░████████╗██╗░░░██╗  ░██████╗░██████╗░░█████╗░██████╗░██╗░░██╗"
        putStrLn "██╔══██╗██╔══██╗██╔══██╗██╔══██╗██╔════╝██╔══██╗╚══██╔══╝╚██╗░██╔╝  ██╔════╝░██╔══██╗██╔══██╗██╔══██╗██║░░██║"
        putStrLn "██████╔╝██████╔╝██║░░██║██████╔╝█████╗░░██████╔╝░░░██║░░░░╚████╔╝░  ██║░░██╗░██████╔╝███████║██████╔╝███████║"
        putStrLn "██╔═══╝░██╔══██╗██║░░██║██╔═══╝░██╔══╝░░██╔══██╗░░░██║░░░░░╚██╔╝░░  ██║░░╚██╗██╔══██╗██╔══██║██╔═══╝░██╔══██║"
        putStrLn "██║░░░░░██║░░██║╚█████╔╝██║░░░░░███████╗██║░░██║░░░██║░░░░░░██║░░░  ╚██████╔╝██║░░██║██║░░██║██║░░░░░██║░░██║"
        putStrLn "╚═╝░░░░░╚═╝░░╚═╝░╚════╝░╚═╝░░░░░╚══════╝╚═╝░░╚═╝░░░╚═╝░░░░░░╚═╝░░░  ░╚═════╝░╚═╝░░╚═╝╚═╝░░╚═╝╚═╝░░░░░╚═╝░░╚═╝"
        putStrLn ""
        
        putStrLn "Add the filenames for the files containing the Rho,Lambda,Sigma and Prop functions"
        putStrLn "Remember: they must be in the current working directory"
        putStrLn ""
        
        threadDelay 500000
        
        putStrLn "Input your Rho filename "
        rhofilename <- getLine
        putStrLn "Input your Lambda filename "
        lambdafilename <- getLine
        putStrLn "Input your Sigma filename "
        sigmafilename <- getLine
        putStrLn "Input your Prop filename "
        propfilename <- getLine
        
        putStrLn ""
        putStrLn "POPULATING PROPERTY GRAPH . . ."
        putStrLn ""
        
        threadDelay 500000
        
        pg <- populate rhofilename lambdafilename sigmafilename propfilename
        
        putStrLn ""
        putStrLn "DONE"
        putStrLn ""
        
        threadDelay 500000
        
        showGraph pg
        
        threadDelay 1000000
        
        putStrLn ""
        putStrLn "ADDING EDGE n3->n5"
        putStrLn ""
        
        let pgEdge = addEdge pg "newedge" "n3" "n5"
        
        threadDelay 1000000
        
        showGraph pgEdge
        
        -- test updates AND COMMENT THE LABELING
        
        let mpg = fromMaybe pgEdge (defElabel pgEdge ("newedge","n3","n5") "tuco")
        
        showGraph mpg
        
        let res = propE mpg 2 "canSing"
        
        putStrLn $ show res
        
        putStrLn "end"
        
        
