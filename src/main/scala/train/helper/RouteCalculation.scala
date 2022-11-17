package train.helper

import train.helper.Routes.TrainRoutes

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

case class RouteValidation(nodes: String, validated: Boolean)
case class RouteDist(nodes: String, distance: Int)

object RouteCalculation {
  def distanceWithProvidedRoute(trainRoutes: TrainRoutes, routeDashSeparated: String): Int = {
    val routeArray = routeDashSeparated.split('-')
    val routePairs = routeArray.zipWithIndex.dropRight(1).map( routeWithIdx => routeWithIdx._1 + routeArray(routeWithIdx._2 + 1))

    routePairs.foldLeft(0){ (dist, pair) => {
      if (trainRoutes.exists( route =>
        route._1.contains(pair.head.toString) && route._2.exists(destinationDist => destinationDist._1.contains(pair.tail)))
      ) {
        dist + trainRoutes(pair.head.toString)(pair.tail)
      } else {
        throw new Exception("No Such Route")
      }
    }}
  }

  @tailrec
  def routesWithMaxStops(start: String, end: String, maxStops: Int, allRoutes: TrainRoutes, routeAccOpt: Option[List[RouteValidation]]): List[String] = {
    routeAccOpt match
      case None =>
        val routeAcc = allRoutes(start).keys.map(nextNode => RouteValidation(start + nextNode, false))
        routesWithMaxStops(start, end, maxStops, allRoutes, Some(routeAcc.toList))

      case Some(routeAcc) =>
        if (routeAcc.isEmpty) {
          throw new Exception("No routes found")
        } else {

          if (routeAcc.exists(routeValid=> !routeValid.validated)) {
            val updatedRouteValid = routeAcc.foldLeft(List.empty[RouteValidation])((acc, routeValid) => {

              if (routeValid.validated) {
                acc :+ routeValid
              } else {
                val lastNode = routeValid.nodes.last.toString
                val possibleNextNodes = allRoutes(lastNode)

                val newRoutes = possibleNextNodes.keys.map(node => RouteValidation(routeValid.nodes + node, false))
                  .filter(newRoute => newRoute.nodes.length <= (maxStops + 1))
                  .toList

                if (lastNode.equals(end)) {
                  (acc :+ RouteValidation(routeValid.nodes, true)) ++ newRoutes
                } else {
                  acc ++ newRoutes
                }
              }
            })

            routesWithMaxStops(start, end, maxStops, allRoutes, Some(updatedRouteValid))
          } else {
            routeAcc.map(_.nodes)
          }
        }
  }

  def routesWithExactStops(start: String, end: String, maxStops: Int, allRoutes: TrainRoutes, routeAccOpt: Option[List[RouteValidation]]): List[String] = {
    val routesWithMax = routesWithMaxStops(start, end, maxStops, allRoutes, routeAccOpt)
    routesWithMax.filter(_.length.equals(maxStops + 1))
  }

  @tailrec
  def shortestRoute(start: String, end: String, allRoutes: TrainRoutes, routeAccOpt: Option[List[RouteDist]], optimalRoute: Option[(String, Int)]): (String, Int) = {
    routeAccOpt match
      case None =>
        val nextNodes = allRoutes(start)
        val possibleRoute = nextNodes.find(_._1.equals(end))
        val routeAcc = nextNodes
          .filter(!_._1.equals(end))
          .filter(destinationDist => possibleRoute.forall(_._2 >= destinationDist._2))
          .map(destinationDist => RouteDist(start + destinationDist._1, destinationDist._2))

        shortestRoute(start, end, allRoutes, Some(routeAcc.toList), possibleRoute)

      case Some(routeAcc) =>
        if (routeAcc.isEmpty) {
          optimalRoute match
            case None =>
              throw new Exception("No routes found")
            case Some(route) =>
              route
        } else {
          var tempShortestRoute = optimalRoute
          val updatedRoutes: List[RouteDist] = routeAcc.foldLeft(List.empty[RouteDist])((acc, routeDist) => {
            val lastNode = routeDist.nodes.last.toString
            val possibleNextNodes = allRoutes(lastNode)

            val possibleOptimalRoute = possibleNextNodes
              .find(_._1.equals(end))
              .map(optimalRoute => (routeDist.nodes + optimalRoute._1, routeDist.distance + optimalRoute._2))

            val updatedOptimalRouteOpt = (possibleOptimalRoute, tempShortestRoute) match
              case (Some(newRoute), Some(currentRoute)) =>
                if (newRoute._2 < currentRoute._2) {
                  tempShortestRoute = Some(newRoute)
                  tempShortestRoute
                } else {
                  Some(currentRoute)
                }
              case (Some(newRoute), None) =>
                tempShortestRoute = Some(newRoute)
                tempShortestRoute
              case (None, Some(currentRoute)) =>
                Some(currentRoute)
              case _ =>
                None

            val routeAcc = possibleNextNodes
              .filterNot(_._1.equals(end))
              .filter(destinationDist => updatedOptimalRouteOpt.forall(_._2 > destinationDist._2))
              .map(destinationDist => RouteDist(routeDist.nodes + destinationDist._1, routeDist.distance + destinationDist._2))
              .filter(updatedAcc => updatedOptimalRouteOpt.forall(updatedOptimalRoute => !updatedOptimalRoute._1.equals(updatedAcc.nodes) && updatedAcc.distance < updatedOptimalRoute._2))

            acc ++ routeAcc
          })
          shortestRoute(start, end, allRoutes, Some(updatedRoutes), tempShortestRoute)
        }
  }

  @tailrec
  def routesWithMaxDist(start: String, end: String, maxDistance: Int, allRoutes: TrainRoutes, routeAccOpt: Option[List[RouteDist]], acceptedRoutes: List[String]): List[String] = {
    routeAccOpt match
      case None =>
        val nextNodes = allRoutes(start)
        val possibleRoute = nextNodes.find(_._1.equals(end))
          .filter(_._2 <= maxDistance)
          .map(routeDist => List(routeDist._1))
          .getOrElse(List.empty)
        val routeAcc = nextNodes
          .filter(_._2 <= maxDistance)
          .map(destinationDist => RouteDist(start + destinationDist._1, destinationDist._2))

        routesWithMaxDist(start, end, maxDistance, allRoutes, Some(routeAcc.toList), possibleRoute)

      case Some(routeAcc) =>
        if (routeAcc.isEmpty) {
          if (acceptedRoutes.isEmpty)
              throw new Exception("No routes found")
          else
            acceptedRoutes
        } else {
          val newAcceptedRoutes = ListBuffer[String]()
          val updatedAccRoutes = routeAcc.foldLeft(List.empty[RouteDist])((acc, routeDist) => {
            val lastNode = routeDist.nodes.last.toString
            val possibleNextNodes = allRoutes(lastNode)

            val routeAcc = possibleNextNodes
              .map(nextNode => RouteDist(routeDist.nodes + nextNode._1, routeDist.distance + nextNode._2))
              .filter(updatedRoute => updatedRoute.distance <= maxDistance)
              .toList

            val routesWithEndNode = routeAcc.filter(_.nodes.last.toString.equals(end)).map(_.nodes)
            newAcceptedRoutes ++= routesWithEndNode

            acc ++ routeAcc
          })
          
          routesWithMaxDist(start, end, maxDistance, allRoutes, Some(updatedAccRoutes), acceptedRoutes ++ newAcceptedRoutes)
        }
  }
}
