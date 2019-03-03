import ru.ekuzmichev.fp_in_scala.chapter4._
def mean(xs: Seq[Double]): Option[Double] =
  if (xs.isEmpty) None
  else Some(xs.sum / xs.length)

mean(Seq())
mean(Seq(1,2,3))