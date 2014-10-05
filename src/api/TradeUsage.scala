package api

import brokerageDSL.FixedIncome
import java.util.Date
import brokerageDSL.FixedIncome
import brokerageDSL.Currency
import api.FixedIncomeTrade
import brokerageDSL.USD
import brokerageDSL.Equity
import brokerageDSL.CouponBond

case class FixedIncomeTradeImpl(
		val tradingAccount: Account,
		val instrument: FixedIncome,
		val currency: Currency,
		val tradeDate: Date = null,
		val market: Market,
		val quantity: BigDecimal,
		val unitPrice: BigDecimal) extends FixedIncomeTrade
		object client {
	def usage() = {
		val fi = null
				val t1 =
				FixedIncomeTradeImpl(
						tradingAccount = fi,
						instrument = fi,
						currency = USD,
						market = NYSE,
						quantity = 100,
						unitPrice = 10)
	}}