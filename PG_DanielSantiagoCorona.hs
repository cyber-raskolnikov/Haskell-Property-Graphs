import System.IO
import Data.List
import Control.Monad
import Data.Maybe

{-|
        NOT-SO-RANDOM NOTES:
        ->  the existence of isolated nodes is not considered,
        that is, I'm taking as {V u E} the elements of the rho file.
        
        -> about returning the Val type on the query functions,
        I have implemented the Val type in such a way that when printing (show function) the result of the queries,
        it will be displayed correctly.
        
        -> by requirement, populate has to use IO, 
        this forces us to return IO PG in populate, slightly deviating from the specification demands
        
        -> overall, some slight deviations from the practice task demands have been implemented,
        as the specification is somewhat loose and teachers have pointed out it should be treated
        as a non-strict guideline
        
        - I have forced myself to use first-order functions in several algorithms, so the program has a more functional tune,
        there is also a heave use of comprehension lists, as I like to think in terms of those
        
        -> the Date type is treated as a String
        
        -> a (small) concession has been taken on the 120-characters long code lines so I could add some ASCII art
        
        -> the practice task description asked that 'For each query evaluated users must enter the real parameters and the result must print.'
        but after a conversation with the responsible teacher, this requirement was dropped as long as we generated an informative test
        
        -> some typos were corrected from the given test files, so if you want to replicate the test results,
        PLEASE EXECUTE THE SCRIPT WITH THE PROVIDED TEST FILES
        
-}

--------------------------------------------------------------------------------------------- 
---------------------------------------------------------------------------------------------
---------TYPE SYNONYMS---------
--------------------------------------------------------------------------------------------- 
---------------------------------------------------------------------------------------------

type Label = String

type Property = String

type Identifier = String -- this type definition is useful, as it allows to refer to both nodes and edges

type Node = Identifier

type Edge =  (Identifier,Node,Node)

type PropertyAndValue = (Identifier,Property,Val)

type ElementAndLabel = (Identifier,Label)

type PG = ([Node],[Edge],[ElementAndLabel],[PropertyAndValue])

--------------------------------------------------------------------------------------------- 
---------------------------------------------------------------------------------------------
-------DATA DECLARATIONS-------
--------------------------------------------------------------------------------------------- 
---------------------------------------------------------------------------------------------

data Val = IntValue Int | DecValue Double | Text String | Binary Bool deriving Eq

--------------------------------------------------------------------------------------------- 
---------------------------------------------------------------------------------------------
-----INSTANCE DECLARATIONS-----
--------------------------------------------------------------------------------------------- 
---------------------------------------------------------------------------------------------

instance Show Val where
        show (IntValue int) = show int
        show (DecValue double) = show double
        show (Text text) = show text
        show (Binary bool) = show bool
       
--------------------------------------------------------------------------------------------- 
---------------------------------------------------------------------------------------------
------AUXILIARY FUNCTIONS------
---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------

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

propLookup :: Identifier -> Property -> [PropertyAndValue] -> Maybe Val

-- given an indentificator, a property and the list that maps ids to their properties & values
-- returns the value for the property corresponding to the given id

propLookup _ prop [] = Nothing
propLookup id prop ((currId,currProp,currVal):rest)
                | (id == currId && prop == currProp) = Just currVal
                | otherwise = propLookup id prop rest

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

defProp (nodes,edges,labels,prop) (propId,propName,propVal) = (nodes,edges,labels,propMinus++[(propId,propName,propVal)])
                                                where propMinus = [(id,name,val) | (id,name,val) <- prop,
                                                                                 (propId /= id || propName /= name)]

--

defLabel :: PG -> ElementAndLabel -> Maybe PG

defLabel (nodes,edges,labels,prop) (id,label)
                          | labelLookup id labels == "\0" = Just (nodes,edges,labels ++ [(id,label)],prop)
                          | otherwise = Nothing
                                                
--------------------------------------------------------------------------------------------- 
---------------------------------------------------------------------------------------------
---------PG FUNCTIONS----------
--------------------------------------------------------------------------------------------- 
---------------------------------------------------------------------------------------------

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
                                
--------------------------------------------------------------------------------------------- 
---------------------------------------------------------------------------------------------
--------QUERY FUNCTIONS--------
--------------------------------------------------------------------------------------------- 
---------------------------------------------------------------------------------------------

sigmaPrima :: PG -> Identifier -> [(Property,Val)]

-- given a property graph and an identifier
-- it returns the set of properties and values related to the identifier

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
                                             
--

reachable_aux :: PG -> [Node] -> Label -> Node -> Node -> Bool

-- given a property graph, a label and existing ending and starting nodes (MIND THE ORDER)
-- returns the boolean value stating whether a path exists from the starting to ending node
-- where all edge's labels are the given label
-- while taking into account a visited nodes list

reachable_aux (nodes,edges,labels,properties) exclusionList label end start
        | end == start = True
        | otherwise = or ( map (reachable_aux (nodes,edges,labels,properties) visitedNodes label end) possibleNodes )
                        where possibleNodes = [posNode | (edgeid,start_aux,posNode) <- edges,
                                                         (edgeid2,label_aux) <- labels,
                                                         (start_aux == start 
                                                         && label == label_aux
                                                         && edgeid == edgeid2 
                                                         && not (elem posNode exclusionList))]
                              visitedNodes = exclusionList++possibleNodes++[start]
                              
reachable :: PG -> Label -> Node -> Node -> Bool

-- given a property graph, a label and existing ending and starting nodes 
-- returns the boolean value stating whether a path exists from the starting to ending node
-- where all edge's labels are the given label

reachable (nodes,edges,labels,properties) label start end = 
        reachable_aux (nodes,edges,labels,properties) [] label end start
        
--

kHops :: PG -> Int -> Property -> (Val -> Val -> Bool) -> Val -> Node -> [(Node,Label,Val)]

-- given a property graph, a number k of steps, a property, a function that takes 2 Val and outputs a boolean, a Val value and a starting node,
-- it returns a set of (end node, their labels, their values for said property)
-- given that a k-path exists from the starting node and each end node 
-- AND that the function taking the Val value and the value for said property on the end node returns True

kHops (nodes,edges,labels,properties) steps prop func referenceVal currNode
        | steps == 0 && (isNothing valcurrNode || not (func referenceVal (fromJust valcurrNode))) = []
        | steps == 0 && (func referenceVal (fromJust valcurrNode)) = [(currNode,labcurrNode,(fromJust valcurrNode))]
        | otherwise = map head . group $ concatMap 
        (kHops (nodes,edges,labels,properties) (steps-1) prop func referenceVal) neighbors
        where labcurrNode = labelLookup currNode labels
              valcurrNode = propLookup currNode prop properties
              neighbors = [adjNode | (_,start_aux,adjNode) <- edges,
                                     start_aux == currNode]


--------------------------------------------------------------------------------------------- 
---------------------------------------------------------------------------------------------
---------MAIN FUNCTION---------
--------------------------------------------------------------------------------------------- 
---------------------------------------------------------------------------------------------

main :: IO ()

main = do
        putStrLn ""
        putStrLn "██████╗ ██████╗  ██████╗ ██████╗ ███████╗██████╗ ████████╗██╗   ██╗     ██████╗ ██████╗  █████╗ ██████╗ ██╗  ██╗███████╗██████╗ "
        putStrLn "██╔══██╗██╔══██╗██╔═══██╗██╔══██╗██╔════╝██╔══██╗╚══██╔══╝╚██╗ ██╔╝    ██╔════╝ ██╔══██╗██╔══██╗██╔══██╗██║  ██║██╔════╝██╔══██╗"
        putStrLn "██████╔╝██████╔╝██║   ██║██████╔╝█████╗  ██████╔╝   ██║    ╚████╔╝     ██║  ███╗██████╔╝███████║██████╔╝███████║█████╗  ██████╔╝"
        putStrLn "██╔═══╝ ██╔══██╗██║   ██║██╔═══╝ ██╔══╝  ██╔══██╗   ██║     ╚██╔╝      ██║   ██║██╔══██╗██╔══██║██╔═══╝ ██╔══██║██╔══╝  ██╔══██╗"
        putStrLn "██║     ██║  ██║╚██████╔╝██║     ███████╗██║  ██║   ██║      ██║       ╚██████╔╝██║  ██║██║  ██║██║     ██║  ██║███████╗██║  ██║"
        putStrLn "╚═╝     ╚═╝  ╚═╝ ╚═════╝ ╚═╝     ╚══════╝╚═╝  ╚═╝   ╚═╝      ╚═╝        ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝     ╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝"
        putStrLn ""
        
        putStrLn "If you can't visualize the ASCII art on top of this line, resize your CLI."
        putStrLn ""
        putStrLn "Add the filenames for the files containing the Rho,Lambda,Sigma and Prop functions"
        putStrLn "Remember: they must be in the current working directory"
        putStrLn ""
        
        
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
        
        
        pg <- populate rhofilename lambdafilename sigmafilename propfilename
        
        putStrLn ""
        putStrLn "DONE:"
        putStrLn ""
        
        showGraph pg
        
        --
        
        putStrLn "TESTING THE PROPERTY GRAPH STRUCTURAL FUNCTIONS\n"
        
        putStrLn "TESTING ADDEDGE:"
        putStrLn "Adding a non-existing edge between existing nodes: n1 --> n10"
        let pg1 = addEdge pg "testedge1" "n1" "n10"
        
        putStrLn "Adding a non-existing edge between non-existing nodes: Dani --> Guido van Rossum"
        let pg2 = addEdge pg1 "testedge2" "Dani" "GuidovanRossum"
        
        showGraph pg2
        putStrLn "\nSuccess: Both edges were added and in the case where the nodes didn't exist they were created"
        
        --
        
        putStrLn "\nTESTING DEFVPROP AND DEFEPROP"
        putStrLn "Adding a value for a non-defined property of a node"
        let pg3 = defVprop pg2 "Dani" ("homeTown",Text "Saigon")
        putStrLn "Adding a value for a non-defined property of an edge"
        let pg4 = defEprop pg3 ("testedge2","Dani","GuidovanRossum") ("since",Text "05-06-2018")
        putStrLn "Updating a value for a defined property of a node"
        let pg5 = defVprop pg4 "n1" ("homeTown",Text "Hanoi")
        putStrLn "Updating a value for a defined property of an edge"
        let pg6 = defEprop pg5 ("f1","n5","n7") ("since",Text "24-11-2021")
        
        showGraph pg6
        putStrLn "Success: The nodes and edges properties were successfully set/updated"
        
        --
        
        putStrLn "\nTESTING DEFVLABEL AND DEFELABEL"
        putStrLn "Adding a label for a non-labeled node : Dani[Person]"
        let pg7 = defVlabel pg6 "Dani" "Person"
        if isNothing pg7 then print "ERROR" else showGraph $ fromJust pg7
        let pg77 = fromMaybe pg6 (defVlabel pg6 "Dani" "Person")
        putStrLn "Adding a label for a non-labeled edge : Dani --follows--> GuidovanRossum"
        let pg8 = defElabel pg77 ("testedge2","Dani","GuidovanRossum") "follows"
        if isNothing pg8 then print "ERROR" else showGraph $ fromJust pg8
        let pg88 = fromMaybe pg77 (defElabel pg77 ("testedge2","Dani","GuidovanRossum") "follows")
        
        putStrLn "Adding a label to a labeled node : Dani[Person]"
        let pg9 = defVlabel pg88 "Dani" "Pperson"
        if isNothing pg9 then print "ERROR" else showGraph $ fromJust pg9
        let pg99 = fromMaybe pg88 (defVlabel pg88 "Dani" "Person")
        putStrLn "Adding a label to a labeled edge : Dani --follows--> GuidovanRossum"
        let pg10 = defElabel pg99 ("testedge2","Dani","GuidovanRossum") "follows"
        if isNothing pg10 then print "ERROR" else showGraph $ fromJust pg10
        let pg1010 = fromMaybe pg99 (defElabel pg99 ("testedge2","Dani","GuidovanRossum") "follows")
        putStrLn "\nSuccess: As expected, one can only label unlabeled nodes/edges"
        
        --
        putStrLn ""
        putStrLn "TESTING THE PROPERTY GRAPH QUERY FUNCTIONS"
        putStrLn "We will restart to the original test files Property Graph"
        qpg <- populate rhofilename lambdafilename sigmafilename propfilename
        showGraph qpg
        
        putStrLn "TESTING THE SIGMA PRIMA FUNCTION"
        putStrLn "We want to retrieve the Property and Value pairs for node n5 and for edge e1 (has none)\n"
        
        putStrLn $ "node n5: " ++ (show $ sigmaPrima qpg "n5")
        putStrLn $ "edge e1: " ++ (show $ sigmaPrima qpg "e1")
        
        --
        
        putStrLn "\nTESTING THE PROPV AND PROPE FUNCTION\n"
        putStrLn $ "first 4 firstName of nodes: " ++ (show $ propV qpg 4 "firstName")
        putStrLn $ "first 42 lastName of nodes: " ++ (show $ propV qpg 42 "lastName")
        putStrLn $ "first 2 testing(non-existent) of edges: " ++ (show $ propE qpg 2 "testing")
        putStrLn $ "first 5 date of edges: " ++ (show $ propE qpg 4 "date")
        
        --
        
        putStrLn "\nTESTING THE K-HOPS FUNCTION"
        putStrLn "In order to have a function to use with it, I have given the Val type the == operator\n"
        
        putStrLn $ "Females reachable from n1 in 3 jumps : " ++
         (show $ kHops qpg 3 "gender" (==) (Text "female") "n1")
        putStrLn $ "Males reachable from n1 in 4 jumps : " ++
         (show $ kHops qpg 4 "gender" (==) (Text "male") "n1")
        putStrLn $ "Spaniards/Spanish Things reachable from n1 in 1 jumps : " ++
         (show $ kHops qpg 1 "country" (==) (Text "Spain") "n1")
        
        --
        
        putStrLn "\nTESTING THE REACHABLE FUNCTION"
        
        putStrLn $ "Is n7 reachable from n1 by the 'follows' tag? : " ++
         (show $ reachable qpg "follows" "n1" "n7")
        putStrLn $ "Is n1 reachable from n7 by the 'follows' tag? : " ++
         (show $ reachable qpg "follows" "n7" "n1")
        putStrLn "The two previous tests indicate it can treat with loops\n"
        
        putStrLn $ "Is n8 reachable from n9 by the 'follows' tag? (n9 doesn't follow anyone) : " ++
         (show $ reachable qpg "follows" "n9" "n8")
        
        putStrLn "\nEND OF THE TEST"
        
        
