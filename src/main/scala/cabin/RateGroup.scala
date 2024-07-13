package cabin

object RateGroup {

  def main(): Unit = {
    val rates = Seq(
      Rate("M1", "Military"),
      Rate("M2", "Military"),
      Rate("S1", "Senior"),
      Rate("S2", "Senior")
    )

    val prices = Seq(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "M2", 250.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "S2", 260.00),
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CB", "S1", 245.00),
      CabinPrice("CB", "S2", 270.00)
    )

    val bestGroupPrices = getBestGroupPrices(rates, prices)

    println(bestGroupPrices)
  }

  //  The problem we’ll be focusing on for this exercise will be finding the best price for a particular rate group.

  //  Cabin Price: The price for a specific cabin on a specific cruise. All cabin prices will have a single rate attached.
  case class CabinPrice(
     cabinCode: String,
     rateCode: String,
     price: BigDecimal
   )

  //  Rate: A rate is a way to group related prices together.
  //    A rate is defined by its Rate Code and which Rate Group it belongs to.
  //    For example. (MilAB, Military) and (Sen123, Senior)
  case class Rate(
     rateCode: String,
     rateGroup: String
   )

  //  Rate Group: Specific rates are grouped into a related rate group.
  //    There is a one-to-many relationship between rate groups and rates
  //    (A rate group is made up of many rates, but a rate can only belong
  //    to a single rate group) Some examples of rate groups are:
  //    Standard, Military, Senior, and Promotion.

  case class BestGroupPrice(
    cabinCode: String,
    rateCode: String,
    price: BigDecimal,
    rateGroup: String
  )

  //  1. Write a function that will take a list of rates and a list of prices
  //  and returns the best price for each rate group. We’ve supplied the function
  //  and case class definitions below for you to use.

  val UNKNOWN_RATE_GROUP = "__UNKNOWN_RATE_GROUP"
  // Good opportunities for pattern matching? optional types?

  def getBestPriceForCabin(rateGroup: String)(prices: Seq[CabinPrice]): BestGroupPrice = {
    val bestGroupPrice = prices.minBy(_.price)

    BestGroupPrice(
      cabinCode = bestGroupPrice.cabinCode,
      rateCode = bestGroupPrice.rateCode,
      price = bestGroupPrice.price,
      rateGroup = rateGroup
    )
  }

  def getBestPriceForCabinsInGroup(rateGroup: String, groupPrices: Seq[CabinPrice]): Seq[BestGroupPrice] = {
    def getBestCabinPrice = getBestPriceForCabin(rateGroup)  _

    groupPrices
      .groupBy(_.cabinCode)
      .map {
        case (_, cabinPrices) =>
          getBestCabinPrice(cabinPrices)
      }.toSeq
  }

  def getBestGroupPrices(
    rates: Seq[Rate],
    prices: Seq[CabinPrice]
  ): Seq[BestGroupPrice] = {
    val rateCodeToGroupMap = rates
      .map(r => (r.rateCode, r.rateGroup))
      .toMap

    prices
      .groupBy(p => rateCodeToGroupMap.getOrElse(p.rateCode, UNKNOWN_RATE_GROUP)) // what if we're provided a rate code that doesn't exist in the rates list?
      .flatMap {
        case (rateGroup, pricesByGroup) =>
          getBestPriceForCabinsInGroup(rateGroup, pricesByGroup)
      }
      .toSeq
      .sortBy(_.price)
  }

}
