package basics

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import Basics._

class BasicsSpec extends AnyFlatSpec {
  "lcm" should "work" in {
    lcm(12, 18) shouldEqual Some(36)
    lcm(8, 12) shouldEqual Some(24)
  }

  "gcd" should "work" in {
    gcd(24, 18) shouldEqual 6
  }
}
