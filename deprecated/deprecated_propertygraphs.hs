import System.IO  
import Control.Monad

data Val = IntValue Int | DecValue Double | Text String | Binary Bool -- | Date

type Label = String--Maybe String -- IF problematic drop the maybe

type Property = String

type PropertyAndValue = (Property,Val)

type Node = String

type Edge =  (Node,Node)

data PGEntry = NodeEntry Node Label [PropertyAndValue] | EdgeEntry Edge Label [PropertyAndValue]

type PG = [PGEntry]

instance Show PGEntry where
        show (NodeEntry node label _) = " Node: " ++ node ++ " label: " ++ label
        show (EdgeEntry (n1,n2) label _) = " Edge going from " ++ n1 ++ " to " ++ n2 ++ " with label " ++ label

--test filename = do
--                        contents <- readFile filename
--                        print (map words $ lines contents)
                        
--makeNodes

makeNodes [_,n1,n2] = [NodeEntry n1 "" [], NodeEntry n2 "" []]

--makeEdges

makeEdges "DANI te has quedado aqui, la movida es implementar esto CONSCIENTE de que los nodos ya existen"
--version de prueba donde te da igual?

treatLineNodes :: [String] -> [PGEntry]

treatLineNodes lines = concat (map (makeNodes . words) lines)

treatEdgeCases :: [String] -> [PGEntry]

treatEdgeCases lines = map (makeEdges . words) lines

--basicpopulate :: String -> PG--String -> String -> String -> String -> PG

basicpopulate filename = do
                                contents <- readFile filename
                                let inputLines = lines contents
                                let treatedLineCases = treatLineNodes inputLines
                                let treatedEdgeCases = treatEdgeCases inputLines
                                --treatedEdgeCases
                                print(treatedEdgeCases)

--main = do
--        let list = []
--        contents <- readFile "input1.txt"
--        print (words contents)
