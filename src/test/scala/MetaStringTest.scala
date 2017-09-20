import org.scalatest.{FlatSpec, Matchers}

class MetaStringTest extends FlatSpec with Matchers {

  "areMetaStrings" should "return true" in {
      MetaStrings.areMetaStrings("viktors","vrktois") should be (true)
      MetaStrings.areMetaStrings("abava","abvaa") should be (true)
  }

  "areMetaStrings" should "return false" in {
    MetaStrings.areMetaStrings("lambda","lambda") should be (false)
    MetaStrings.areMetaStrings("true","fals") should be (false)
    MetaStrings.areMetaStrings("bear","beer") should be (false)
    MetaStrings.areMetaStrings("asdf","asdfg") should be (false)
    MetaStrings.areMetaStrings("esdfg","asdfe") should be (false)
  }
}
