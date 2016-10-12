package qa

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait TestCase extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks
