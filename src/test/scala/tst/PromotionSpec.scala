package tst

import tst.Promotion._
import org.scalatest._
import flatspec._
import matchers._

class PromotionSpec extends AnyFlatSpec with should.Matchers {

  it should "find all PromotionCombos provided by TST team" in {
    val output = allCombinablePromotions(inputProvidedByTstTeam)

    val expected = Seq(
      PromotionCombo(Seq("P1", "P2")),
      PromotionCombo(Seq("P1", "P4", "P5")),
      PromotionCombo(Seq("P2", "P3")),
      PromotionCombo(Seq("P3", "P4", "P5"))
    )

    output should equal(expected)

  }

  it should "find all PromotionCombos related to P1" in {

    val output = combinablePromotions("P1", inputProvidedByTstTeam)

    val expected = Seq(
      PromotionCombo(Seq("P1", "P2")), PromotionCombo(Seq("P1", "P4", "P5"))
    )

    output should equal(expected)

  }

  it should "find all PromotionCombos related to P3" in {
    val input = Seq(
      Promotion("P1", Seq("P3")), // P1 is not combinable with P3
      Promotion("P2", Seq("P4", "P5")), // P2 is not combinable with P4 and P5
      Promotion("P3", Seq("P1")), // P3 is not combinable with P1
      Promotion("P4", Seq("P2")), // P4 is not combinable with P2
      Promotion("P5", Seq("P2")) // P5 is not combinable with P2
    )

    val output = combinablePromotions("P3", input)

    val expected = Seq(
      PromotionCombo(Seq("P3", "P2")),
      PromotionCombo(Seq("P3", "P4", "P5"))
    )

    output should equal(expected)

  }

  it should "throw an exception when promotion compatibility is conflicting" in {
    val input = Seq(
      Promotion("P1", Seq("P3", "P4", "P5")),
      Promotion("P2", Seq("P1", "P4", "P5")),
      Promotion("P3", Seq("P1", "P2", "P5")),
      Promotion("P4", Seq("P1", "P2", "P3")),
      Promotion("P5", Seq("P2", "P3", "P4"))
    )

    a [IllegalStateException] should be thrownBy {
      allCombinablePromotions(input)
    }

  }
}
