module GUI where

import open Graphics.Input
import Point2D (Point2D)
import Point2D as P
import Graph     (Vector2D, NodeID, EdgeID, Node, Edge, Graph)
import Graph     as Gr
import List      as L
import Dict      (Dict)
import Dict      as D
import Set       (Set)
import Set       as S
import Color     as C


data Tool = Select | Drag (Maybe Node)
data Mode = Simulation | Edit Tool [Node]
type ProgramState = { graph : Graph, mode : Mode }
type Dimensions a = { width : a, height : a, dimensions : (a,a) }

dimensions2 width height = Dimensions width height (width,height)
dimensions1 size = let d = dimensions2 size size in { d | size = size }


border : LineStyle -> Element -> Element
border lineStyle element =
  let width  = widthOf element
      height = heightOf element
  in collage width height <| [outlined lineStyle <| rect (toFloat width) (toFloat height), toForm <| element]


colors = { node = { normal = C.blue, hover = C.lightBlue }
         , edge = { normal = C.red,  hover = C.lightRed  }
         , collage = rgb 245 245 245
         }


buttonSize = 40
nodeRadius = 3


scene dims ((programState,hoverNodes) as ps') =
  let win   = dims.window
      bBar  = dims.buttonBar
      rArea = dims.renderArea
      pan   = dims.panel
  in container win.width win.height midTop <| (buttonBar programState.mode bBar.width bBar.height `above` renderArea ps' rArea.size) `beside` panel hoverNodes pan.width pan.height

dimensionsFromWindow (winWidth, winHeight) =
  let (height, panelWidth, renderAreaSize) =
      let fromWidth = dimensionsFromWidth winWidth
          (h,_,_) = fromWidth
      in if h <= winHeight then fromWidth else dimensionsFromHeight winHeight
  in { window     = dimensions2 winWidth winHeight
     , buttonBar  = dimensions2 renderAreaSize buttonSize
     , renderArea = dimensions1 renderAreaSize
     , panel      = dimensions2 panelWidth height
     }

dimensionsFromWidth width =
  let panelWidth = floor <| (*) 0.2 <| toFloat width
      renderAreaSize = width - panelWidth
      height = renderAreaSize + buttonSize
  in (height, panelWidth, renderAreaSize)

dimensionsFromHeight height =
  let renderAreaSize = height - 20
      panelWidth = floor <| (*) 0.25 <| toFloat renderAreaSize
  in (height, panelWidth, renderAreaSize)

(simulate, buttonPlay, buttonPause) =
  let source = buttons False
      play   = source.button True  "Play"
      pause  = source.button False "Pause"
  in (source.events, play, pause)

data Buttons = ButtonSelect | ButtonMove | ButtonRotate

(buttonsSignal, buttonList) =
  let source = buttons ButtonSelect
  in (source.events, map (uncurry source.button) <| map (\b -> (b,drop 6 <| show b)) [ButtonSelect, ButtonMove, ButtonRotate])

buttonBar : Mode -> Int -> Int -> Element
buttonBar mode width height =
  let firstButton = if mode == Simulation then buttonPause else buttonPlay
      buttonArea content = border defaultLine <| container width height midLeft content
  in buttonArea <| flow right <| firstButton :: buttonList

panel : [Node] -> Int -> Int -> Element
panel hoverNodes width height = border defaultLine <| container width height topLeft <| flow down <| L.map (plainText . (.label)) hoverNodes


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

collageCenter dimensions =
  let x = (toFloat (dimensions.window.width - dimensions.panel.width)) / 2
      y = (toFloat dimensions.buttonBar.height) + ((toFloat dimensions.renderArea.size) / 2)
  in P.cartesian (x, y)

relativeMousePosition dimensions posV = let
    fromCenter p = P.e_min p <| collageCenter dimensions
    negateY = P.e_mul <| P.cartesian (1, -1)
  in negateY <| fromCenter <| posV

renderAreaCollage : Int -> [Form] -> Element
renderAreaCollage size = border defaultLine . collage size size

renderArea : (ProgramState, [Node]) -> Int -> Element
renderArea (programState, hoverNodes) size =
  let hoverNodeEdges = L.foldl (\n acc -> S.union n.edges <| S.union n.bEdges acc) S.empty hoverNodes

      ncolor nid = if any (\hn -> nid == hn.nid) hoverNodes then colors.node.hover else colors.node.normal
      ecolor eid = if S.member eid hoverNodeEdges           then colors.edge.hover else colors.edge.normal

      drawnGraph = renderAreaCollage size <| drawGraph programState.graph ncolor ecolor

      modeText = case programState.mode of
        Simulation -> "Simulation"
        Edit _ _   -> "Edit"
  in layers [drawnGraph, plainText <| "Mode: " ++ modeText]
