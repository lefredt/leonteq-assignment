package train.helper

object Routes {
  /**
   * Complete train routes are stored with the below structure
   * Exmaple routes of "AB5, BC4, CD8" would be maintained as
   * Map("A"-> Map("B" -> 5), "B"-> Map("C" -> 4), "C"-> Map("D" -> 8))
   */
  type TrainRoutes = Map[String, Map[String, Int]]

  def collateAll(routeList: List[String]): TrainRoutes = {
    routeList.foldLeft(Map.empty[String, Map[String, Int]])((acc, route) => {
      val newRouteFirstLetter = route.head.toString
      val newRouteSecondLetter = route.charAt(1).toString
      val newRouteDistance = route.charAt(2).toString.toInt

      // Check if the starting location already exists in the TrainRoutes Map
      if (acc.exists(_._1.equals(newRouteFirstLetter))) {
        val existingRouteWithStartKey: Map[String, Int] = acc(newRouteFirstLetter)
        // Check if destination exists
        if (existingRouteWithStartKey.exists(_._1.equals(newRouteSecondLetter))) {
          throw new Exception(s"Duplicated route $route")
        } else {
          val updatedRouteForFirstLetter = existingRouteWithStartKey + (newRouteSecondLetter -> newRouteDistance)
          acc - newRouteFirstLetter + (newRouteFirstLetter -> updatedRouteForFirstLetter)
        }
      } else {
        acc + (newRouteFirstLetter -> Map(newRouteSecondLetter -> newRouteDistance))
      }
    })
  }

  def removeOne(routes: TrainRoutes, route: String): TrainRoutes = {
    val routeFirstLetter = route.head.toString
    val routeSecondLetter = route.charAt(1).toString

    if (
      routes.exists(startToDestinations => {
        startToDestinations._1.contains(routeFirstLetter) &&
          startToDestinations._2.exists(_._1.equals(routeSecondLetter))
      })
    ) {
      // Remove route from all the Routes
      val currentDestinationDistances = routes(routeFirstLetter)
      routes - routeFirstLetter + (routeFirstLetter -> (currentDestinationDistances - routeSecondLetter))
    } else {
      throw new Exception("No Route Found")
    }
  }
}
