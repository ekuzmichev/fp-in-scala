case class CreditCard(name: String)

case class Charge(cc: CreditCard, amount: Double) {
  def combine(other: Charge) = {
    if (cc == other.cc) {
      Charge(cc, amount + other.amount)
    } else {
      throw new Exception
    }
  }
}

class Coffee() {
  def price: Double = 10.0

  override def toString: String = "Coffee cup"
}

class Cafe {
  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = new Coffee()
    (cup, Charge(cc, cup.price))
  }

  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
    val purchases = List.fill(n)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip
    (coffees, charges.reduce((c1, c2) => c1.combine(c2)))
  }

  def coalesce(charges: List[Charge]): List[Charge] = {
    charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toList
  }
}

val cafe = new Cafe()
val cardVisa = new CreditCard("VISA")
val cardMasterCard = new CreditCard("MASTER CARD")
val cardMaestro = new CreditCard("MAESTRO")
val (cup, charge) = cafe.buyCoffee(cardVisa)
val (cups, totalCharge) = cafe.buyCoffees(cardVisa, 10)
val charges = cafe.coalesce(
  List(
    Charge(cardVisa, 20.0),
    Charge(cardVisa, 30.0),
    Charge(cardMasterCard, 10.0),
    Charge(cardMasterCard, 10.0),
    Charge(cardMaestro, 20.0)
  )
)

val x = "123"
x.reverse