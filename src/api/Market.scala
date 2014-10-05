package api

sealed class Market (name: String)

case object NYSE extends Market("New York Stock Exchange")