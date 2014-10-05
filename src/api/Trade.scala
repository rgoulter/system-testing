package api

package api
import java.util.Date
import brokerageDSL.FixedIncome
import brokerageDSL.Equity
import brokerageDSL.Currency
import brokerageDSL.Instrument
trait Trade {
	def tradingAccount: Account
	def instrument: Instrument
	def currency: Currency
	def tradeDate: Date
	def unitPrice: BigDecimal
	def quantity: BigDecimal
	def market: Market
	def principal = unitPrice * quantity
	var cashValue: BigDecimal = _
	var taxes: Map[TaxFee, BigDecimal] = _
}

// Specializations of the trade trait to allow specialized financial instruments to be returned
trait FixedIncomeTrade extends Trade {
	override def instrument: FixedIncome
	var accruedInterest: BigDecimal = _
}

trait EquityTrade extends Trade {
	override def instrument: Equity
}
