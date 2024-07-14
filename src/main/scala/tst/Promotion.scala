package tst

import cats.implicits.catsSyntaxSemigroup

object Promotion {

  def main(args: Array[String]): Unit = {

    println("\n1. All combinable promotions")
    println(allCombinablePromotions(inputProvidedByTstTeam))

    println("\n2. Matches for P1")
    println(combinablePromotions("P1", inputProvidedByTstTeam))

    println("\n3. Matches for P3")
    println(combinablePromotions("P3", inputProvidedByTstTeam))
  }

  val inputProvidedByTstTeam = Seq(
    Promotion("P1", Seq("P3")), // P1 is not combinable with P3
    Promotion("P2", Seq("P4", "P5")), // P2 is not combinable with P4 and P5
    Promotion("P3", Seq("P1")), // P3 is not combinable with P1
    Promotion("P4", Seq("P2")), // P4 is not combinable with P2
    Promotion("P5", Seq("P2")) // P5 is not combinable with P2
  )

  case class Promotion(
    code: String,
    notCombinableWith: Seq[String]
  )

  case class PromotionCombo(
    promotionCodes: Seq[String]
  )

  object PromotionCombo {

    def withLeading(promotionCode: String)(otherCodes: Seq[String]): PromotionCombo =
      PromotionCombo((promotionCode :: otherCodes.toList).sorted)

    def appointLeader(promotionCode: String)(combo: PromotionCombo): PromotionCombo =
      PromotionCombo(promotionCode +: combo.promotionCodes.filter(_ != promotionCode))
  }

  // Implement a function to find all PromotionCombos with maximum number of
  // combinable promotions in each. The function and case class definitions
  // are supplied below to get you started.

  private def testCompatibilityBetweenPromotions(
    promotionCode: String,
    compatibilityMap: Map[String, Seq[String]]
  )(
    promotionCodes: Seq[String]
  ): Boolean =
    compatibilityMap
      .get(promotionCode)
      .exists(compatibleCodes =>
        promotionCodes.forall(compatibleCodes.contains)
      )

  private def promotionCodeAndSizeOrdering(a: PromotionCombo, b: PromotionCombo): Boolean =
    (a.promotionCodes.size, b.promotionCodes.size, a.promotionCodes.head, b.promotionCodes.head) match {
      case (_, _, aHead, bHead) if aHead != bHead => aHead < bHead
      case (aSize, bSize, _, _) => aSize < bSize
    }

  private def allCombinableCodesFor(
    promotion: Promotion
  )(
    promotions: Seq[Promotion]
  ): Seq[String] = {
    def combinableWith(other: Promotion): Boolean =
        !(promotion.code +: promotion.notCombinableWith)
          .contains(other.code)

    promotions
      .filter(combinableWith)
      .map(_.code)
  }

  private def combinableCodesByPromotion(allPromotions: Seq[Promotion]): Map[String, Seq[String]] = {
    val compatibilityMap = allPromotions.map(p => (
      p.code,
      allCombinableCodesFor(p)(allPromotions)
    )).toMap

    compatibilityMap.foreach {
      case (promotionCode, combinablePromotionCodes) =>
        combinablePromotionCodes
          .find(code =>
            !compatibilityMap
              .getOrElse(code, List.empty)
              .contains(promotionCode)
          ) match {
          case None => ()
          case Some(misconfiguredCombinablePromotions) =>
            throw new IllegalStateException(s"Bad promotion configuration: $promotionCode reported $misconfiguredCombinablePromotions as combinable but $misconfiguredCombinablePromotions reported $promotionCode as non-combinable. ")
        }
    }

    compatibilityMap
  }

  def allCombinablePromotions(
     allPromotions: Seq[Promotion]
  ): Seq[PromotionCombo] = {
    val compatibilityMap = combinableCodesByPromotion(allPromotions)

    def buildPromotionCombosGivenCompatibility(
     head: String,
     rangeFrom: => Range,
     sliceFrom: Int => Seq[String],
     handleMissingSplitIdx: () => Option[Seq[Seq[String]]]
   ): Option[Seq[Seq[String]]] = {
        val testCompatibility = testCompatibilityBetweenPromotions(head, compatibilityMap) _

        rangeFrom
          .find(splitIdx =>
            testCompatibility(sliceFrom(splitIdx))
          ) match {
          case Some(splitIdx) => Some(
            List(head +: sliceFrom(splitIdx))
          )
          case None => handleMissingSplitIdx()
        }

    }

    def combosFor(codes: Seq[String]): Option[Seq[Seq[String]]] = codes match {
      case Nil =>
        None
      case only +: Nil =>
        Some(Seq(Seq(only)))
      case head +: others =>
        def tryCodesFromRight(): Option[Seq[Seq[String]]] = buildPromotionCombosGivenCompatibility(
          head = head,
          rangeFrom = others.size - 1 to 0 by -1,
          handleMissingSplitIdx = () => None,
          sliceFrom = (idx: Int) => others.take(idx)
        ) |+| combosFor(others)

        buildPromotionCombosGivenCompatibility(
          head = head,
          rangeFrom = others.indices,
          sliceFrom = (idx: Int) => others.drop(idx),
          handleMissingSplitIdx = tryCodesFromRight,
        )
      }

    val results = for {
      (promotionCode, combinableCodes) <- compatibilityMap
      combos = combosFor(combinableCodes)
      if combos.nonEmpty
    } yield
      combos.get.map(PromotionCombo.withLeading(promotionCode))

      results
      .flatten
      .toSet
      .toList
        .sortWith(promotionCodeAndSizeOrdering)

  }


  //  2. Implement a function to find all PromotionCombos for a given
  //  Promotion from given list of Promotions. The function definition is provided.
  def combinablePromotions(
    promotionCode: String,
    allPromotions: Seq[Promotion]
  ): Seq[PromotionCombo] = {
    val compatibilityMap = combinableCodesByPromotion(allPromotions)

    val maybeRelatedPromotionCodeSet = compatibilityMap
      .get(promotionCode)
      .map(compatibleCodes => (promotionCode +: compatibleCodes).toSet)

    maybeRelatedPromotionCodeSet match {
        case Some(relatedPromotionCodes) => allCombinablePromotions(
          allPromotions
            .filter(p => relatedPromotionCodes.contains(p.code))
        ).map(PromotionCombo.appointLeader(promotionCode))

        case _ => Seq.empty
      }
  }

}
