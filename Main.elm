{- |
Module        :  Main
Description   :  Definition and drawing of a graph.
Copyright     :  (c) Jeff Smits
License       :  GPL-3.0

Maintainer    :  jeff.smits@gmail.com
Stability     :  experimental
Portability   :  portable
Compatibility :  The Elm Compiler (pre-)0.9

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

module Main where

import List      as L
import Dict (Dict)
import Dict      as D
import Set (Set)
import Set       as S
import Maybe     as M
--import Graphics  as G
import Color     as C
import Automaton as A
import Point2D (Point2D)
import Point2D   as P
import General (Vector2D, NodeID, EdgeID, Node, Edge, Graph)
import General   as Gen
import Physics   as Ph
import TGF (TGFNode, TGFEdge, TGFGraph)
import TGF
import Keyboard
import Mouse

data Mode = Simulation | Edit (Maybe Node)
type ProgramState = { graph : Graph, mode : Mode }

-- frameRate : FPS
fr = 30
-- checkFrameRate : FPS -> Bool
cfr r = r < (fr / 2)
-- collageSize
cs = Gen.cs
-- clamp collage
cc = clamp 0 cs
-- collageCenter
ccenter = cs `div` 2


colors = { node = { normal = C.blue, hover = C.cyan    }
         , edge = { normal = C.red,  hover = C.magenta }
         , collage = rgb 245 245 245
         }

headToMaybeAndList : [a] -> (Maybe a, [a])
headToMaybeAndList l = case l of
  h :: t -> (Just h, [h])
  []     -> (Nothing, [])

editGraph : Bool -> Point2D -> [Node] -> ProgramState -> (ProgramState, [Node])
editGraph mouseDown mouseRelPos hoverNodes programState = let
    noNodeDrag = let
        (mSelectedNode, lSelectedNode) = headToMaybeAndList hoverNodes
        newMode = Edit (if mouseDown then mSelectedNode else Nothing)
        newState : ProgramState
        newState = { programState | mode <- newMode }
      in (newState, lSelectedNode)
  in case programState.mode of
    Simulation           -> noNodeDrag
    Edit Nothing         -> noNodeDrag
    Edit (Just dragNode) -> let
        hoverNode = [dragNode]

        -- decide if still dragging
        newMode = Edit (if mouseDown then Just dragNode else Nothing)
        -- update drag node position
        newDragNode = { dragNode | pos <- mouseRelPos }
        newNodes = D.insert dragNode.nid newDragNode programState.graph.nodes
        g = programState.graph -- compiler can't handle dots (.) in graph syntax yet :(
        newGraph = { g | nodes <- newNodes }

        newProgramState = { graph = newGraph, mode = newMode }
      in (newProgramState, hoverNode)

drawGraph : Graph -> (NodeID -> Color) -> (EdgeID -> Color) -> [Form]
drawGraph g ncolor ecolor = (drawEdges g ecolor) ++ (drawNodes g ncolor)

drawNodes : Graph -> (NodeID -> Color) -> [Form]
drawNodes g ncolor = let
    -- node to form
    n2f n = drawNode (ncolor n.nid) n.pos
  in (L.map n2f <| D.values g.nodes)

-- draw a node with color c and position p
drawNode : Color -> Point2D -> Form
drawNode c p = circle Gen.nodeRadius |> filled c |> move (p.x, p.y)

-- draw an edge with color c from p1 to p2
drawEdge : Color -> Point2D -> Point2D -> Form
drawEdge c p1 p2 = segment (p1.x, p1.y) (p2.x, p2.y) |> traced (solid c)

drawEdges : Graph -> (EdgeID -> Color) -> [Form]
drawEdges g ecolor = let
    edge2form _ e acc =
        case (D.lookup e.idFrom g.nodes, D.lookup e.idTo g.nodes) of
            (Just n1, Just n2) -> (drawEdge (ecolor e.eid) n1.pos n2.pos) :: acc
            _                  -> acc
  in  (D.foldl edge2form [] g.edges)

relativeMousePosition : Point2D -> Point2D
relativeMousePosition posV = let
    fromCenter p = P.min p (toFloat ccenter)
    negateY = P.e_mul <| P.point2D 1 (0-1)
  in negateY <| fromCenter <| posV

layoutCollage : [Form] -> Element
layoutCollage = color colors.collage . collage cs cs

layout : (ProgramState, [Node]) -> Element
layout (programState, hoverNodes) =
  let hoverNodeEdges = L.foldl (\n acc -> S.union n.edges <| S.union n.bEdges acc) S.empty hoverNodes

      ncolor nid = if any (\hn -> nid == hn.nid) hoverNodes then colors.node.hover else colors.node.normal
      ecolor eid = if S.member eid hoverNodeEdges           then colors.edge.hover else colors.edge.normal

      info = flow down <| L.map (plainText . (.label)) hoverNodes
      drawnGraph = layoutCollage <| drawGraph programState.graph ncolor ecolor

      modeText = case programState.mode of
        Simulation -> "Simulation"
        Edit _     -> "Edit"
  in layers [drawnGraph, plainText <| "Mode: " ++ modeText] `beside` info


seconds : Signal Float
seconds = keepIf cfr fr <| inSeconds <~ fps fr

simulate : Signal Bool
simulate = Gen.toggle True Keyboard.space

step : Bool -> Float -> Bool -> Point2D -> ProgramState -> (ProgramState, [Node])
step render timeDelta mouseDown relMousePos programState = let
    hoverNodes = Gen.nodeAt programState.graph <| relMousePos
  in if render
    then ({ graph = Ph.physicsStep timeDelta programState.graph, mode = Simulation }, hoverNodes)
    else editGraph mouseDown relMousePos hoverNodes programState

transform : Signal (ProgramState -> (ProgramState, [Node]))
transform = step <~ simulate ~ seconds ~ Mouse.isDown ~ (relativeMousePosition . P.cartesian . (\(a,b) -> (toFloat a, toFloat b)) <~ Mouse.position)


test1 : Graph
test1 = {nodes = D.fromList [ (1,{nid=1,label="1",pos={x=15,y=15},vel={x=0,y=0},edges=S.fromList [1],bEdges=S.fromList [2]})
                         , (2,{nid=2,label="2",pos={x=35,y= 5},vel={x=0,y=0},edges=S.fromList [3],bEdges=S.fromList [1]})
                         , (3,{nid=3,label="3",pos={x=15,y=15},vel={x=0,y=0},edges=S.fromList [2],bEdges=S.fromList [3]})
                         ], edges = D.fromList [ (1,{eid=1,idFrom=1,idTo=2,label="1"})
                                               , (2,{eid=2,idFrom=3,idTo=1,label="2"})
                                               , (3,{eid=3,idFrom=2,idTo=3,label="3"})
                                               ]}

test2 : Graph
test2 = Gen.createGraph { nodes = [{id=1,label="1"},{id=2,label="2"},{id=3,label="3"},{id=4,label="4"}], edges = [{idFrom=1,idTo=2,label="1"},{idFrom=3,idTo=1,label="2"},{idFrom=2,idTo=3,label="3"},{idFrom=1,idTo=4,label="4"},{idFrom=2,idTo=4,label="5"},{idFrom=4,idTo=3,label="6"}] }

-- from: http://docs.yworks.com/yfiles/doc/developers-guide/tgf.html
test3 : Graph
test3 = Gen.createGraph <| TGF.fromString """1 January
2 March
3 April
4 May
5 December
6 June
7 September
#
1 2
3 2
4 3
5 1 Happy New Year!
5 3 April Fools Day
6 3
6 1
7 5
7 6
7 1"""

programState : Signal (ProgramState, [Node])
programState = foldp (\transformFun (graph,_) -> transformFun graph) ({graph=test3,mode=Simulation}, []) transform

main : Signal Element
main = layout <~ programState
