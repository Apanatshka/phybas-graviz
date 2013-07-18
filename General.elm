{- |
Module      :  General
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

module General where

import List    as L
import Dict    as D
import Set     as S
import Maybe   as M
import Point2D as P

type Point2D  = {x : Float, y : Float}
type Vector2D = Point2D

type NodeID = Int
type EdgeID = Int

type TGFNode  = { id : NodeID, label : String }
type TGFEdge  = { idFrom : NodeID, idTo : NodeID, label : String }
type TGFGraph = { nodes : [TGFNode], edges : [TGFEdge] }

type Node  = { nid : NodeID, label : String, pos : Point2D, vel : Vector2D, edges : Set EdgeID, bEdges : Set EdgeID }
type Edge  = { eid : EdgeID, idFrom : NodeID, idTo : NodeID, label : String }
type Graph = { nodes : Dict NodeID Node, edges : Dict EdgeID Edge }

nodeRadius = 3

-- collageSize
cs = 500

-- sign : Number -> Number
sign num = if num == 0 then 1 else num / abs num

-- signedPower = sp : Number -> Number
sp num pow = if pow == 0 then sign num else num * abs (num ^ (pow-1))

-- signedSquare = ss : Number -> Number
ss num = num * abs num

-- precision or frictionFactor
ff = 1000000

-- small_value
small_value = 1 / ff / ff

-- negate a number
neg n = 0 - n


-- when input signal turns true, the toggle changes
toggle : Bool -> Signal Bool -> Signal Bool
toggle start input = fst <~ foldp (\inputIsTrue (toggleVal,inputWasTrue) ->
  (if (not inputWasTrue) && inputIsTrue then not toggleVal else toggleVal, inputIsTrue)) (start, False) input


createGraph : TGFGraph -> Graph
createGraph g = let
    -- tuples of edge-ids and edges
    edges' = L.map (\(i,e) -> (i,{e|eid=i})) <| L.zip [1..length g.edges] g.edges

    -- code to set up initial positions of nodes
    gridSize  = ceiling <| sqrt <| toFloat <| length g.nodes
    gridPos s i =
      let gridPoint = P.point2D (i `mod` gridSize) (i `div` gridSize)
          space p   = P.mul p s
      in space <| gridPoint

    -- TGF nodes to nodes
    tgfn2n n = (n.id, { nid    = n.id
                      , label  = n.label
                      , pos    = gridPos (nodeRadius*4) n.id
                      , vel    = P.from1D 0
                      , edges  = S.fromList <| L.map fst <| L.filter (\(_,e) -> e.idFrom == n.id) edges'
                      , bEdges = S.fromList <| L.map fst <| L.filter (\(_,e) -> e.idTo   == n.id) edges'
                      })

    -- the actual graph
    newNodes = D.fromList <| L.map tgfn2n g.nodes
    newEdges = D.fromList edges'
  in { nodes = newNodes, edges = newEdges }

nodeAt : Graph -> Point2D -> [Node]
nodeAt g mpos = let
    overlap n = let
        dist = P.magn <| P.e_min n.pos mpos
      in dist < nodeRadius
  in L.filter overlap <| D.values g.nodes

-- delta of the position of two nodes plus a small pseudorandom value
positionDelta : Node -> Node -> Point2D
positionDelta from to = {-
  let d a b = a / b -- workaround for Int is not Float error..
      r   = P.point2D (from.nid `d` to.nid) (to.nid `d` (to.nid - from.nid))
      srv = P.mul r small_value -- small (pseudo-)random value
  in P.e_pls srv <|-} P.e_min to.pos from.pos
