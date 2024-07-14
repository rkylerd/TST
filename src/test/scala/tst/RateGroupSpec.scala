package tst

import tst.RateGroup._
import org.scalatest._
import flatspec._
import matchers._

class RateGroupSpec extends AnyFlatSpec with should.Matchers {
  it should "find the best group prices provided by TST team" in {

    val bestGroupPrices = getBestGroupPrices(ratesProvidedByTstTeam, pricesProvidedByTstTeam)
    val expectedOutput = Seq(
      BestGroupPrice("CA", "M1", 200.00, "Military"),
      BestGroupPrice("CA", "S1", 225.00, "Senior"),
      BestGroupPrice("CB", "M1", 230.00, "Military"),
      BestGroupPrice("CB", "S1", 245.00, "Senior"),
    )
    bestGroupPrices should equal(expectedOutput)

  }

  it should "handle and include prices for unknown rates" in {
    val rates = Seq(
      Rate("S2", "Senior")
    )

    // Notice all the pricing info related to rate groups that
    // weren't provided in the rates "3rd-party" request results
    val prices = pricesProvidedByTstTeam

    val bestGroupPrices = getBestGroupPrices(rates, prices)
    val expectedOutput = Seq(
      BestGroupPrice("CA", "M1", 200.00, UNKNOWN_RATE_GROUP),
      BestGroupPrice("CB", "M1", 230.00, UNKNOWN_RATE_GROUP),
      BestGroupPrice("CA", "S2", 260.00, "Senior"),
      BestGroupPrice("CB", "S2", 270.00, "Senior"),
    )
    bestGroupPrices should equal(expectedOutput)
  }

  it should "handle fewer prices than rates" in {

    val prices = Seq(
      CabinPrice("CB", "S2", 270.00)
    )

    val bestGroupPrices = getBestGroupPrices(ratesProvidedByTstTeam, prices)
    val expectedOutput = Seq(
      BestGroupPrice("CB", "S2", 270.00, "Senior"),
    )
    bestGroupPrices should equal(expectedOutput)
  }
}
