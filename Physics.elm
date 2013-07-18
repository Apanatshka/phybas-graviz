{- |
Module      :  Physics
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

module Physics where

import Dict    as D
import Set     as S
import Point2D as P
import General as Gen

type Point2D  = {x : Float, y : Float}
type Vector2D = Point2D

type NodeID = Int
type EdgeID = Int

type Node  = { nid : NodeID, label : String, pos : Point2D, vel : Vector2D, edges : Set EdgeID, bEdges : Set EdgeID }
type Edge  = { eid : EdgeID, idFrom : NodeID, idTo : NodeID, label : String }
type Graph = { nodes : Dict NodeID Node, edges : Dict EdgeID Edge }

-- nodeConstants = { mass,   radius }
nc               = { m = 10, r = Gen.nodeRadius }

-- forceConstants = { repulsion,   springConstant (in N/m), equilibriumLength }
fc                = { r = 1000000, sc = 50,                 el = 60           }

-- maximum speed
ms = Gen.cs/4
-- clamp maximum speed
cms = clamp (0-ms) ms

repulsion : Graph -> Node -> Vector2D
repulsion g n =
  let r _ cn f =
    if cn.nid == n.nid
      then f
      else
        let d = Gen.positionDelta n cn
            (l,u) = P.breakDown d
            f' = 0 - (fc.r / l^2)
        in P.e_pls f <| P.mul u f'
  in D.foldl r P.zero g.nodes

nodeStep : Float -> Graph -> Graph
nodeStep delta g =
  let mrf  = D.map (repulsion g) g.nodes
      appRep n =
        let appRep' f =
          let (l,u) = P.breakDown f
              vl = l * delta / nc.m
              v = P.mul u <| cms vl
          in{ n | vel <- P.e_pls n.vel v }
        in maybe n appRep' <| D.lookup n.nid mrf
  in { g | nodes <- D.map appRep g.nodes }

attraction : Graph -> Node -> Vector2D
attraction g n =
  let a _ cn f =
    if cn.nid == n.nid
      then f
      else
        let ei = S.union (S.intersect n.edges cn.bEdges) (S.intersect n.bEdges cn.edges)
        in
          if S.toList ei == []
          then f
          else
            let d = Gen.positionDelta n cn
                (l,u) = P.breakDown d
                f' = fc.sc * (l - fc.el)
            in P.e_pls f <| P.mul u f'
  in D.foldl a P.zero g.nodes

edgeStep : Float -> Graph -> Graph
edgeStep delta g =
  let maf  = D.map (attraction g) g.nodes
      appAttr n =
        let appAttr' f =
          let (l,u) = P.breakDown f
              vl = l * delta / nc.m
              v = P.mul u <| cms vl
          in{ n | vel <- P.e_pls n.vel v }
        in maybe n appAttr' <| D.lookup n.nid maf
  in { g | nodes <- D.map appAttr g.nodes }

drag : Graph -> Node -> Vector2D
drag _ n =
  let (l,u) = P.breakDown n.vel

      rho = 998.2071        -- kg/m^3 (20 degrees Celsius water [2])
      dF  = 1/3000          -- extra density factor, for tweaking

      dens  = dF * rho      -- kg/m^3, density
      coeff = 0.47          -- dimensionless (coefficient for a circular shape [3])
      area  = pi * nc.r ^ 2 -- m^2, area

      f = 0-(l^2 * dens * coeff * area / 2) -- N
  in P.mul u f

dragStep : Float -> Graph -> Graph
dragStep delta g =
  let mdf  = D.map (drag g) g.nodes
      appDrag n =
        let appDrag' f =
          let (l,u) = P.breakDown f
              vl = l * delta / nc.m
              vl' = clamp 0 (P.magn n.vel) vl--if vl > P.magn n.vel then P.magn n.vel else vl
              v = P.mul u vl'
          in{ n | vel <- P.e_pls n.vel v }
        in maybe n appDrag' <| D.lookup n.nid mdf
  in { g | nodes <- D.map appDrag g.nodes }

velocityStep : Float -> Graph -> Graph
velocityStep delta g =
  let vel  n       = { n | pos <- vel2 n.pos n.vel }
      vel2 pos vel = P.e_pls pos (P.mul vel delta)
  in { g | nodes <- D.map vel g.nodes }

frictionStep : Float -> Graph -> Graph
frictionStep delta g =
  let fric  n   = { n | vel <- fric2 n.vel }
      fric2 vel = P.map fric3 vel
      -- simple rounding off to ff amount of digits after the dot.
      fric3 v   = toFloat (round <| Gen.ff * v) / Gen.ff
  in { g | nodes <- D.map fric g.nodes }


physicsStep : Float -> Graph -> Graph
physicsStep d = if d == 0 then id else velocityStep d . dragStep d . edgeStep d . nodeStep d
