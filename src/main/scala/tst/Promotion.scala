package tst

import cats.implicits.catsSyntaxSemigroup
import tst.Promotion.allPromotionsAreCompat

object Promotion {


  case class Promotion(
    code: String,
    notCombinableWith: Seq[String]
  )

  case class PromotionCombo(
    promotionCodes: Seq[String]
  )

  object PromotionCombo {
    def withLeadingCode(leader: String)(otherCodes: Seq[String]): PromotionCombo =
      PromotionCombo((leader :: otherCodes.toList).sorted)
  }

  // Implement a function to find all PromotionCombos with maximum number of
  // combinable promotions in each. The function and case class definitions
  // are supplied below to get you started.

  // find possible links

  def allPromotionsAreCompat(currentCode: String, compatibilityMap: Map[String, Seq[String]])(promotionCodes: Seq[String]): Boolean =
//    promotionCodes match {
//    case Nil =>
//      false
//    case _last +: Nil =>
//      true
//    case head +: others =>
      compatibilityMap
        .get(currentCode)
        .exists(compatibleCodes =>
          promotionCodes.forall(compatibleCodes.contains)
        )

//  }

  private def promotionCodeAndSizePriority(a: PromotionCombo, b: PromotionCombo) =
    (a.promotionCodes.size, b.promotionCodes.size, a.promotionCodes.head, b.promotionCodes.head) match {
      case (_, _, aHead, bHead) if aHead != bHead => aHead < bHead
      case (aSize, bSize, _, _) => aSize < bSize
    }

  def getCombinablePromotionCodesFor(promotion: Promotion)(promotions: Seq[Promotion]): Seq[String] = {
    def isIncompatible(other: Promotion): Boolean =
      promotion.code == other.code ||
        promotion.notCombinableWith.exists(_ == other.code)

    promotions
      .filterNot(isIncompatible)
      .map(_.code)
  }

  def allCombinablePromotions(
     allPromotions: Seq[Promotion]
  ): Seq[PromotionCombo] = {
    val compatibilityMap = allPromotions.map(p => (
      p.code,
      getCombinablePromotionCodesFor(p)(allPromotions)
    )).toMap

    def promotionCombosGivenCompatibility(
     head: String,
     rangeFrom: => Range,
     sliceFrom: (Int) => Seq[String],
     handleMissingSplitIdx: () => Option[Seq[Seq[String]]]
   ): Option[Seq[Seq[String]]] = {
        val withAllCompatibleCodes = allPromotionsAreCompat(head, compatibilityMap) _

        rangeFrom
          .find(splitIdx =>
            withAllCompatibleCodes(sliceFrom(splitIdx))
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
      case head +: others => promotionCombosGivenCompatibility(
        head = head,
        rangeFrom = 0 to others.size - 1,
        sliceFrom = (idx: Int) => others.drop(idx),
        handleMissingSplitIdx = () => promotionCombosGivenCompatibility(
          head = head,
          rangeFrom = (others.size - 1 to 0 by -1),
          handleMissingSplitIdx = () => None,
          sliceFrom = (idx: Int) => others.take(idx)
        ) |+| combosFor(others),
      )
    }

    val results = for {
      x <- compatibilityMap
      combos = combosFor(x._2)
      if combos.nonEmpty
    } yield combos.get.map(PromotionCombo.withLeadingCode(x._1))

      results
      .flatten
      .toSet
      .toList
        .sortWith(promotionCodeAndSizePriority)
        .toSeq


  }


  //  2. Implement a function to find all PromotionCombos for a given
  //  Promotion from given list of Promotions. The function definition is provided.
  def combinablePromotions(
    promotionCode: String,
    allPromotions: Seq[Promotion]
  ): Seq[PromotionCombo] = ???


}
