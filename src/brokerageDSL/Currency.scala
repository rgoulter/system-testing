package brokerageDSL

import java.util.Date;

// This sealed keyword prevents Currency from being inherited anywhere but in this source file
sealed class Currency(code: String)

case object USD extends Currency("US Dollar")

case object JPY extends Currency("Japanese Yen")

case object HKD extends Currency("Hong Kong Dollar")

trait Instrument {
	val isin: String
}


/*
  FixedIncome can be further sub - typed to FixedIncomeSecurity etc.
  therefore it is declared as a trait and not a case class
 */
trait FixedIncome extends Instrument {
	def dateOfIssue: Date
	def dateOfMaturity: Date
	def nominal: BigDecimal
}


case class Equity(isin: String, dateOfIssue: Date) extends Instrument

// These case classes become are value types and not reference. Therefore CouponBond(params) = other CouponBond(params)
case class CouponBond(
		override val isin: String,
		override val dateOfIssue: Date,
		override val dateOfMaturity: Date,
		val nominal: BigDecimal,
		val paymentSchedule: Map[String, BigDecimal])
		extends FixedIncome

case class DiscountBond(
		override val isin: String,
		override val dateOfIssue: Date,
		override val dateOfMaturity: Date,
		val nominal: BigDecimal,
		val percent: BigDecimal)
		extends FixedIncome