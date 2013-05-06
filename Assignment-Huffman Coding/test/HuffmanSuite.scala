package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree t1") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("weight of a larger tree t2") {
	  new TestTrees {
		  assert(weight(t2) === 9)
	  }
  }

  test("chars of a larger tree t2") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("chars of a larger tree t1") {
	  new TestTrees {
		  assert(chars(t1) === List('a','b'))
	  }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times of (\"hello, world\")") {
    val charTimes = times(string2Chars("hello, world"))
	  assert(charTimes.length == 9)
	  assert(charTimes(0)._1 == 'h')
	  assert(charTimes(0)._2 == 1)
	  assert(charTimes(1)._1 == 'e')
	  assert(charTimes(1)._2 == 1)
	  assert(charTimes(2)._1 == 'l')
	  assert(charTimes(2)._2 == 3)
	  assert(charTimes(3)._1 == 'o')
	  assert(charTimes(3)._2 == 2)
	  assert(charTimes(4)._1 == ',')
	  assert(charTimes(4)._2 == 1)
	  assert(charTimes(5)._1 == ' ')
	  assert(charTimes(5)._2 == 1)
	  assert(charTimes(6)._1 == 'w')
	  assert(charTimes(6)._2 == 1)
	  assert(charTimes(7)._1 == 'r')
	  assert(charTimes(7)._2 == 1)
	  assert(charTimes(8)._1 == 'd')
	  assert(charTimes(8)._2 == 1)
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("makeOrderedLeafList for some frequency table of (\"hello, world\")") {
    val charTimes = times(string2Chars("hello, world"))
    val leaves = makeOrderedLeafList(charTimes)
    assert(leaves(7) == Leaf('o', 2))
    assert(leaves(8) == Leaf('l', 3))
  }

  test("singleton test t1") {
    new TestTrees {
    	assert(singleton(List(t1)))
    }
  }

  test("singleton test t1 and t2") {
	  new TestTrees {
		  assert(!singleton(List(t1, t2)))
	  }
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("until test") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val untilList = until(singleton, combine)(leaflist)
    assert(untilList.length == 1)
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
