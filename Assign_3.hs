{- Assignment 3
 - Name: Parsa Zanganeh
 - Date: 2020-11-15
 -}
module Assign_3 where

macid :: String
macid = "zanganep"

{- -----------------------------------------------------------------
 - Datatypes
 - -----------------------------------------------------------------
 -}
type Graph a = [(Node a,Edges)]
type Edges = [NodeID]
type NodeID = Int

data Node a = Node { getNodeID  :: NodeID,
                     getNodeVal :: a }
  deriving (Show,Eq)

{- -----------------------------------------------------------------
 - Example Graph
 - -----------------------------------------------------------------
 -              -----        ----------
 -              | A |------->| C |    |
 -              -----        ----- <---
 -                |           |
 -                |     ------|
 -                v     |
 -              ----- <-|
 -              | B |
 -              -----
 -}
nodeA,nodeB,nodeC :: Node Char
nodeA = Node 0 'A'
nodeB = Node 1 'B'
nodeC = Node 2 'C'

exGraph :: Graph Char
exGraph = [(nodeA,[1,2]),(nodeB,[]),(nodeC,[1,2])]

{- -----------------------------------------------------------------
 - maxNodeID
 - -----------------------------------------------------------------
 - Description: returns the largest NodeID in a Graph
 -}
maxNodeID :: Graph a -> Maybe NodeID
maxNodeID nodes = let 
  maxNodeIDAux nodes maxID = if length nodes==0
    then Just maxID
    else if maxID < getNodeID(fst (head nodes))
      then maxNodeIDAux (tail nodes) (getNodeID(fst(head nodes)))
      else maxNodeIDAux (tail nodes) maxID
  in if length nodes == 0
    then Nothing
    else maxNodeIDAux (tail nodes) (getNodeID(fst(head nodes)))

{- -----------------------------------------------------------------
 - insertNode
 - -----------------------------------------------------------------
 - Description: inserts a new Node with the given value into a Graph
 -}
insertNode :: a -> Graph a -> Graph a
insertNode v graph = if length graph > 0
  then graph ++ [(Node (lastID+1) v,[])]
  else [(Node 0 v,[])]
  where lastID = getNodeID (fst (graph!!(length graph-1)))

{- -----------------------------------------------------------------
 - removeNode
 - -----------------------------------------------------------------
 - Description: removes any Node with the given NodeID from the given Graph
 -}
removeNode :: NodeID -> Graph a -> Graph a
removeNode nodeID graph = if length graph == 0
  then graph
  else if getNodeID (fst (head graph)) == nodeID
    then removeNode nodeID (tail graph)
    else (fst (head graph),filter (/=nodeID) (snd (head graph))):removeNode nodeID (tail graph)

{- -----------------------------------------------------------------
 - lookupNode
 - -----------------------------------------------------------------
 - Description: returns the Node corresponding to the given NodeID in the given Graph
 -}
lookupNode :: NodeID -> Graph a -> Maybe (Node a)
lookupNode nID graph = if length graph == 0
  then Nothing
  else if getNodeID (fst (head graph)) == nID
    then Just (fst (head graph))
    else lookupNode nID (tail graph)

{- -----------------------------------------------------------------
 - insertEdge
 - -----------------------------------------------------------------
 - Description: inserts an edge from the Node with the given NodeID in the first part of the tuple to the Node with the given NodeID in the second part of the tuple
 -}
insertEdge :: Eq a => (NodeID,NodeID) -> Graph a -> Maybe (Graph a)
insertEdge (n1,n2) graph = let
  insertEdgeAux (n1,n2) graph = if getNodeID (fst (head graph)) == n1
    then if elem n2 (snd (head graph))
      then graph
      else (fst (head graph),snd (head graph)++[n2]):tail graph
    else head graph:insertEdgeAux (n1,n2) (tail graph)
  in if lookupNode n1 graph == Nothing || lookupNode n2 graph == Nothing
    then Nothing
    else Just (insertEdgeAux (n1,n2) graph)

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
Function: maxNodeID
Test Case Number: 1
Input: exGraph
Excpected Output: Just 2
Actual Output: Just 2

Function: maxNodeID
Test Case Number: 2
Input: insertNode 'D' exGraph
Excpected Output: Just 3
Actual Output: Just 3

Function: maxNodeID
Test Case Number: 3
Input: []
Excpected Output: Nothing
Actual Output: Nothing

Function: insertNode
Test Case Number: 1
Input: 'D' exGraph
Excpected Output: [(Node {getNodeID = 0, getNodeVal = 'A'},[1,2]),(Node {getNodeID = 1, getNodeVal = 'B'},[]),(Node {getNodeID = 2, getNodeVal = 'C'},[1,2]),(Node {getNodeID = 3, getNodeVal = 'D'},[])]
Actual Output: [(Node {getNodeID = 0, getNodeVal = 'A'},[1,2]),(Node {getNodeID = 1, getNodeVal = 'B'},[]),(Node {getNodeID = 2, getNodeVal = 'C'},[1,2]),(Node {getNodeID = 3, getNodeVal = 'D'},[])]

Function: insertNode
Test Case Number: 2
Input: 'A' []
Excpected Output: [(Node {getNodeID = 0, getNodeVal = 'A'},[])]
Actual Output: [(Node {getNodeID = 0, getNodeVal = 'A'},[])]

Function: insertNode
Test Case Number: 3
Input: 'A' exGraph
Excpected Output: [(Node {getNodeID = 0, getNodeVal = 'A'},[1,2]),(Node {getNodeID = 1, getNodeVal = 'B'},[]),(Node {getNodeID = 2, getNodeVal = 'C'},[1,2]),(Node {getNodeID = 3, getNodeVal = 'A'},[])]
Actual Output: [(Node {getNodeID = 0, getNodeVal = 'A'},[1,2]),(Node {getNodeID = 1, getNodeVal = 'B'},[]),(Node {getNodeID = 2, getNodeVal = 'C'},[1,2]),(Node {getNodeID = 3, getNodeVal = 'A'},[])]

Function: removeNode
Test Case Number: 1
Input: 0 []
Excpected Output: []
Actual Output: []

Function: removeNode 
Test Case Number: 2
Input: 1 exGraph
Excpected Output: [(Node {getNodeID = 0, getNodeVal = 'A'},[2]),(Node {getNodeID = 2, getNodeVal = 'C'},[2])]
Actual Output: [(Node {getNodeID = 0, getNodeVal = 'A'},[2]),(Node {getNodeID = 2, getNodeVal = 'C'},[2])]

Function: removeNode
Test Case Number: 3
Input: 3 exGraph
Excpected Output: [(Node {getNodeID = 0, getNodeVal = 'A'},[1,2]),(Node {getNodeID = 1, getNodeVal = 'B'},[]),(Node {getNodeID = 2, getNodeVal = 'C'},[1,2])]
Actual Output: [(Node {getNodeID = 0, getNodeVal = 'A'},[1,2]),(Node {getNodeID = 1, getNodeVal = 'B'},[]),(Node {getNodeID = 2, getNodeVal = 'C'},[1,2])]

Function: lookupNode
Test Case Number: 1
Input: 0 exGraph
Excpected Output: Just (Node {getNodeID = 0, getNodeVal = 'A'})
Actual Output: Just (Node {getNodeID = 0, getNodeVal = 'A'})

Function: lookupNode
Test Case Number: 2
Input: 3 exGraph
Excpected Output: Nothing
Actual Output: Nothing

Function: lookupNode
Test Case Number: 3
Input: 0 []
Excpected Output: Nothing
Actual Output: Nothing

Function: insertEdge
Test Case Number: 1
Input: (0,0) exGraph
Excpected Output: Just [(Node {getNodeID = 0, getNodeVal = 'A'},[1,2,0]),(Node {getNodeID = 1, getNodeVal = 'B'},[]),(Node {getNodeID = 2, getNodeVal = 'C'},[1,2])]
Actual Output: Just [(Node {getNodeID = 0, getNodeVal = 'A'},[1,2,0]),(Node {getNodeID = 1, getNodeVal = 'B'},[]),(Node {getNodeID = 2, getNodeVal = 'C'},[1,2])]

Function: insertEdge
Test Case Number: 2
Input: (0,1) exGraph
Excpected Output: Just [(Node {getNodeID = 0, getNodeVal = 'A'},[1,2]),(Node {getNodeID = 1, getNodeVal = 'B'},[]),(Node {getNodeID = 2, getNodeVal = 'C'},[1,2])]
Actual Output: Just [(Node {getNodeID = 0, getNodeVal = 'A'},[1,2]),(Node {getNodeID = 1, getNodeVal = 'B'},[]),(Node {getNodeID = 2, getNodeVal = 'C'},[1,2])]

Function: insertEdge
Test Case Number: 3
Input: (0,1) []
Excpected Output: Nothing
Actual Output: Nothing
-}