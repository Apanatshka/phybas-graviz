{- |
Module      :  Graph
Description :  Definition and drawing of a graph.
Copyright   :  (c) Jeff Smits
License     :  GPL-3.0

Maintainer  :  jeff.smits@gmail.com
Stability   :  experimental
Portability :  portable

 | ---------------------------------------------------------------------- |
 | This program is free software: you can redistribute it and/or modify   |
 | it under the terms of the GNU General Public License as published by   |
 | the Free Software Foundation, either version 3 of the License, or      |
 | (at your option) any later version.                                    |
 |                                                                        |
 | This program is distributed in the hope that it will be useful,        |
 | but WITHOUT ANY WARRANTY; without even the implied warranty of         |
 | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          |
 | GNU General Public License for more details.                           |
 |                                                                        |
 | You should have received a copy of the GNU General Public License      |
 | along with this program.  If not, see <http://www.gnu.org/licenses/>.  |
 | ---------------------------------------------------------------------- |
-}

module Graph where

import List    as L
import Dict    (Dict)
import Dict    as D
import Set     (Set)
import Set     as S
import Maybe   as M
import Point2D (Point2D)
import Point2D as P
import TGF     (TGFGraph)

type Vector2D = Point2D

type NodeID = Int
type EdgeID = Int

type Node  = { nid : NodeID, label : String, pos : Point2D, vel : Vector2D, edges : Set EdgeID, bEdges : Set EdgeID }
type Edge  = { eid : EdgeID, idFrom : NodeID, idTo : NodeID, label : String }
type Graph = { nodes : Dict NodeID Node, edges : Dict EdgeID Edge }


createGraph : number -> TGFGraph -> Graph
createGraph spacing g = let
    -- tuples of edge-ids and edges
    edges' = L.map (\(i,e) -> (i,{e|eid=i})) <| L.zip [1..length g.edges] g.edges

    -- code to set up initial positions of nodes
    gridSize  = ceiling <| sqrt <| toFloat <| length g.nodes
    gridPos s i =
      let gridPoint = P.cartesian (toFloat <| i `mod` gridSize, toFloat <| i `div` gridSize)
          space p   = P.mul p s
      in space <| gridPoint

    -- TGF nodes to nodes
    tgfnode2node n = (n.id, Node n.id
                                 n.label
                                 (gridPos spacing n.id)
                                 P.zero
                                 (S.fromList <| L.map fst <| L.filter (\(_,e) -> e.idFrom == n.id) edges')
                                 (S.fromList <| L.map fst <| L.filter (\(_,e) -> e.idTo   == n.id) edges'))

    -- the actual graph
    newNodes = D.fromList <| L.map tgfnode2node g.nodes
    newEdges = D.fromList edges'
  in Graph newNodes newEdges

nodesWithin : number -> Graph -> Point2D -> [Node]
nodesWithin maxDist g mpos = let
    overlap n = let
        dist = P.magn <| P.e_min n.pos mpos
      in dist < maxDist
  in L.filter overlap <| D.values g.nodes
