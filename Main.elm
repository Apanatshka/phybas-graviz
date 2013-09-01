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
import GUI       (ProgramState, Mode, Simulation, Edit, Tool, Select, Drag)
import GUI

import Window
import Keyboard
import Mouse

type SubGraph = { nodes: Set NodeID, edges: Set EdgeID }

frameRate = 30
checkFrameRate r = r < (frameRate / 2)

nodesAt = Gr.nodesWithin GUI.nodeRadius

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

createGraph = let nc = Ph.nodeConstants
              in Gr.createGraph (nc.radius * 4)


seconds : Signal Float
seconds = keepIf checkFrameRate frameRate <| inSeconds <~ fps frameRate

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

{-
modeStep lastMode (sim,but) =
        if sim
          then Simulation
          else case lastMode of
            Simulation              -> Edit Select []
            Edit tool selectedNodes ->
              case but of
                ButtonSelect -> Edit Select selectedNodes
                ButtonMove   -> Edit (Drag Nothing) selectedNodes -- TODO: use Move/Rotate tools
                ButtonRotate -> Edit tool selectedNodes           -- TODO: use Move/Rotate tools
mode : Signal Mode
mode = foldp modeStep (Edit Select []) <| (,) <~ simulate ~ buttonsSignal -}

step : Bool -> Float -> Bool -> Point2D -> ProgramState -> (ProgramState, [Node])
step render timeDelta mouseDown relMousePos programState = let
    hoverNodes = nodesAt programState.graph <| relMousePos
  in if render
    then (ProgramState (Ph.physicsStep timeDelta programState.graph) Simulation, hoverNodes)
    else editGraph mouseDown relMousePos hoverNodes programState

transform : Signal (ProgramState -> (ProgramState, [Node]))
transform = step <~ GUI.simulate ~ seconds ~ Mouse.isDown ~ (GUI.relativeMousePosition <~ dimensions ~ (P.cartesian . (\(a,b) -> (toFloat a, toFloat b)) <~ Mouse.position))


test1 : Graph
test1 = Graph (D.fromList [ (1, Node 1 "1" (P.cartesian (15, 15)) P.zero (S.fromList [1]) (S.fromList [2]))
                          , (2, Node 2 "2" (P.cartesian (35, 5 ))  P.zero (S.fromList [3]) (S.fromList [1]))
                          , (3, Node 3 "3" (P.cartesian (15, 15)) P.zero (S.fromList [2]) (S.fromList [3]))
                          ])
              (D.fromList [ (1, Edge 1 1 2 "1")
                          , (2, Edge 2 3 1 "2")
                          , (3, Edge 3 2 3 "3")
                          ])

test2 : Graph
test2 = createGraph (TGFGraph [TGFNode 1 "1", TGFNode 2 "2", TGFNode 3 "3", TGFNode 4 "4"] [TGFEdge 1 2 "1", TGFEdge 3 1 "2", TGFEdge 2 3 "3", TGFEdge 1 4 "4", TGFEdge 2 4 "5", TGFEdge 4 3 "6"])

-- from: http://docs.yworks.com/yfiles/doc/developers-guide/tgf.html
test3 : Graph
test3 = createGraph <| TGF.fromString """1 January
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

programState' : Signal (ProgramState, [Node])
programState' = foldp (\transformFun (graph,_) -> transformFun graph) ({graph=test3,mode=Simulation}, []) transform

dimensions = GUI.dimensionsFromWindow <~ Window.dimensions

main : Signal Element
main = GUI.scene <~ dimensions ~ programState'
