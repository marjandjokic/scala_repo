package objsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
    val cd = set1.incl(c).incl(d)
    val dc = set1.incl(d).incl(c)
    val set2cd = set2.incl(c).incl(d)
    val mySet1 = set1.incl(new Tweet("a", "a body", 4)).incl(new Tweet("b", "b body", 3)).incl(new Tweet("c", "c body", 2)).incl(new Tweet("d", "d body", 8)).incl(new Tweet("e", "e body", 1))
    val mySet2 = set1.incl(new Tweet("a", "a body", 3)).incl(new Tweet("b", "b body", 4)).incl(new Tweet("c", "c body", 6))
    val mySet3 = set1.incl(new Tweet("a", "a body", 3)).incl(new Tweet("b", "b body", 8)).incl(new Tweet("c", "c body", 6))
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("union: set1 and set2") {
    new TestSets {
      assert(size(set1.union(set2)) === 1)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("mostRetweeted: with two sets set4c") {
	  new TestSets {
		  assert(set4c.mostRetweeted.retweets === 20)
	  }
  }

  test("mostRetweeted: with two sets set4d") {
	  new TestSets {
		  assert(set4d.mostRetweeted.retweets === 20)
	  }
  }

  test("mostRetweeted: with two sets cd") {
	  new TestSets {
		  assert(cd.mostRetweeted.retweets === 9)
	  }
  }

  test("mostRetweeted: with two sets set2cd") {
	  new TestSets {
		  assert(set2cd.mostRetweeted.retweets === 20)
	  }
  }

  test("mostRetweeted: with two sets mySet1") {
	  new TestSets {
		  assert(mySet1.mostRetweeted.retweets === 8)
	  }
  }

  test("mostRetweeted: with two sets mySet2") {
	  new TestSets {
		  assert(mySet2.mostRetweeted.retweets === 6)
	  }
  }
  test("mostRetweeted: with two sets mySet3") {
	  new TestSets {
		  assert(mySet3.mostRetweeted.retweets === 8)
	  }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }

  test("descending: mySet1") {
    new TestSets {
      val trends = mySet1.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "d")
    }
  }

  test("descending: mySet2") {
	  new TestSets {
      val trends = mySet2.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "c")
    }
  }

  test("descending: mySet3") {
	  new TestSets {
      val trends = mySet3.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "b")
    }
  }

  test("googleTweets and appleTweets") {
	  new TestSets {
      assert(size(GoogleVsApple.googleTweets) === 38)
      assert(size(GoogleVsApple.appleTweets) === 150)
    }
  }
}
