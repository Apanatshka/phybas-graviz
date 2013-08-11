{- |
Module        :  Main
Description   :  UI for displaying a graph.
Copyright     :  (c) Jeff Smits
License       :  GPL-3.0

Maintainer    :  jeff.smits@gmail.com
Stability     :  experimental
Portability   :  portable
Compatibility :  The Elm Compiler 0.9

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
import Dict      (Dict)
import Dict      as D
import Set       (Set)
import Set       as S
import Maybe     as M
import Color     as C
import Automaton as A
import Point2D   (Point2D)
import Point2D   as P
import Graph     (Vector2D, NodeID, EdgeID, Node, Edge, Graph)
import Graph     as Gr
import Physics   as Ph
import TGF       (TGFNode, TGFEdge, TGFGraph)
import TGF
import Keyboard
import Mouse

data Tool = Select | Drag (Maybe Node)
data Mode = Simulation | Edit Tool [Node]
type ProgramState = { graph : Graph, mode : Mode }

frameRate = 30
checkFrameRate r = r < (frameRate / 2)

collageSize = 500
collageCenter = collageSize / 2

nodeRadius = 3
nodesAt = Gr.nodesWithin nodeRadius

colors = { node = { normal = C.blue, hover = C.cyan    }
         , edge = { normal = C.red,  hover = C.magenta }
         , collage = rgb 245 245 245
         }

-- when input signal turns true, the toggle changes
toggle : Bool -> Signal Bool -> Signal Bool
toggle start input =
  let toggleFun inputIsTrue (toggleVal,inputWasTrue) =
        (if (not inputWasTrue) && inputIsTrue
           then not toggleVal
           else toggleVal,
        inputIsTrue)
  in fst <~ foldp toggleFun (start, False) input

headToMaybeAndList : [a] -> (Maybe a, [a])
headToMaybeAndList l = case l of
  h :: t -> (Just h, [h])
  []     -> (Nothing, [])


editGraph : Bool -> Point2D -> [Node] -> ProgramState -> (ProgramState, [Node])
editGraph mouseDown mouseRelPos hoverNodes programState = let
    noNodeDrag = let
        (mSelectedNode, lSelectedNode) = headToMaybeAndList hoverNodes
        newMode = if mouseDown then Edit (Drag mSelectedNode) lSelectedNode else Edit (Drag Nothing) []
        newState : ProgramState
        newState = { programState | mode <- newMode }
      in (newState, lSelectedNode)
  in case programState.mode of
    Simulation                            -> noNodeDrag
    Edit (Drag Nothing)         _         -> noNodeDrag
    Edit (Drag (Just dragNode)) selection -> let
        -- update selected nodes position
        relMove = P.e_min mouseRelPos dragNode.pos
        newSelection = L.map (\node -> { node | pos <- P.e_pls node.pos relMove }) selection
        newGraph = L.foldl (\node graph -> { graph | nodes <- D.insert node.nid node graph.nodes }) programState.graph newSelection
        newDragNode = { dragNode | pos <- P.e_pls dragNode.pos relMove }

        -- decide if still dragging
        newMode = Edit (Drag (if mouseDown then Just newDragNode else Nothing)) newSelection

        newProgramState = { graph = newGraph, mode = newMode }
      in (newProgramState, newSelection)

drawGraph : Graph -> (NodeID -> Color) -> (EdgeID -> Color) -> [Form]
drawGraph g ncolor ecolor = (drawEdges g ecolor) ++ (drawNodes g ncolor)

drawNodes : Graph -> (NodeID -> Color) -> [Form]
drawNodes g ncolor = let
    node2form n = drawNode (ncolor n.nid) n.pos
  in (L.map node2form <| D.values g.nodes)

-- draw a node with color c and position p
drawNode : Color -> Point2D -> Form
drawNode c p = circle nodeRadius |> filled c |> move (p.x, p.y)

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
    fromCenter p = P.min p collageCenter
    negateY = P.e_mul <| P.cartesian (1, -1)
  in negateY <| fromCenter <| posV

layoutCollage : [Form] -> Element
layoutCollage = color colors.collage . collage collageSize collageSize

layout : (ProgramState, [Node]) -> Element
layout (programState, hoverNodes) =
  let hoverNodeEdges = L.foldl (\n acc -> S.union n.edges <| S.union n.bEdges acc) S.empty hoverNodes

      ncolor nid = if any (\hn -> nid == hn.nid) hoverNodes then colors.node.hover else colors.node.normal
      ecolor eid = if S.member eid hoverNodeEdges           then colors.edge.hover else colors.edge.normal

      info = flow down <| L.map (plainText . (.label)) hoverNodes
      drawnGraph = layoutCollage <| drawGraph programState.graph ncolor ecolor

      modeText = case programState.mode of
        Simulation -> "Simulation"
        Edit _ _   -> "Edit"
  in layers [drawnGraph, plainText <| "Mode: " ++ modeText] `beside` info


seconds : Signal Float
seconds = keepIf checkFrameRate frameRate <| inSeconds <~ fps frameRate

simulate : Signal Bool
simulate = toggle True Keyboard.space

step : Bool -> Float -> Bool -> Point2D -> ProgramState -> (ProgramState, [Node])
step render timeDelta mouseDown relMousePos programState = let
    hoverNodes = nodesAt programState.graph <| relMousePos
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
test2 = Gr.createGraph (nodeRadius*4) { nodes = [{id=1,label="1"},{id=2,label="2"},{id=3,label="3"},{id=4,label="4"}], edges = [{idFrom=1,idTo=2,label="1"},{idFrom=3,idTo=1,label="2"},{idFrom=2,idTo=3,label="3"},{idFrom=1,idTo=4,label="4"},{idFrom=2,idTo=4,label="5"},{idFrom=4,idTo=3,label="6"}] }

-- from: http://docs.yworks.com/yfiles/doc/developers-guide/tgf.html
test3 : Graph
test3 = Gr.createGraph (nodeRadius*4) <| TGF.fromString """1 January
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
