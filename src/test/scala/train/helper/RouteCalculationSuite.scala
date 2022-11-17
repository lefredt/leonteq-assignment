package train.helper

import train.helper.Routes.TrainRoutes

class RouteCalculationSuite extends munit.FunSuite {
  val trainRoutes: TrainRoutes = Routes.collateAll(List("AB5", "BC4", "CD8", "DC8", "DE6", "AD5", "CE2", "EB3", "AE7"))

  test("distance calculation with actual routes") {
    val distanceABC = RouteCalculation.distanceWithProvidedRoute(trainRoutes, "A-B-C")
    assertEquals(distanceABC, 9)

    val distanceAD = RouteCalculation.distanceWithProvidedRoute(trainRoutes, "A-D")
    assertEquals(distanceAD, 5)

    val distanceADC = RouteCalculation.distanceWithProvidedRoute(trainRoutes, "A-D-C")
    assertEquals(distanceADC, 13)

    val distanceAEBCD = RouteCalculation.distanceWithProvidedRoute(trainRoutes, "A-E-B-C-D")
    assertEquals(distanceAEBCD, 22)
  }

  test("distance calculation with missing routes") {
    interceptMessage[Exception]("No Such Route")(RouteCalculation.distanceWithProvidedRoute(trainRoutes, "A-E-D"))
  }

  test("routes with exact number of stops") {
    val routesWithExactFourStops = RouteCalculation.routesWithExactStops("A", "C", 4, trainRoutes, None)
    assertEquals(routesWithExactFourStops, List("ABCDC", "ADCDC", "ADEBC"))
  }

  test("shortest distance between 2 stations") {
    val shortestAC = RouteCalculation.shortestRoute("A", "C", trainRoutes, None, None)
    assertEquals(shortestAC._2, 9)

    val shortestBB = RouteCalculation.shortestRoute("B", "B", trainRoutes, None, None)
    assertEquals(shortestBB._2, 9)
  }

  test("route with less then input distance") {
    // Method is implemented as less then equals so the input value is 29
    val routesWithMaxDist = RouteCalculation.routesWithMaxDist("C", "C", 29, trainRoutes, None, List.empty)
    assertEquals(routesWithMaxDist, List("CDC", "CEBC", "CDEBC", "CDCEBC", "CEBCDC", "CEBCEBC", "CEBCEBCEBC"))
  }
}
