package main.scala.org.kilcoole.ninetynineproblems.lists

/**
  * A mixture of recursive, tail-recursive and using list methods ignoring error handliing for the most part
  */

object ListProblems {

  def lastElement[T](l: List[T]): T = l match {
    //1: Find the last element of a list
    case x :: Nil => x
    case x :: xs => lastElement(xs)
  }//lastElement

  def penultimateElement[T](l: List[T]): T = l match {
    //2: Find the last but one element of a list
    case x :: xs :: xxs :: Nil => xs
    case x :: xs => penultimateElement(xs)
  }//penultimateElement

  def kthElement[T](l: List[T], k: Int): T = {
    //3: Find the Kth element of a list.
    def _kth(l: List[T],k: Int, acc: Int): T = (l, k, acc) match {
      case (l, k, acc) if k == acc => l.head
      case _ => kthElement(l.tail, k, acc + 1)
    }//_kth
    _kth(l, k, 0)
  }//kthElement

  def lengthList[T](l: List[T]): Int = {
    //4: Find the number of elements of a list.
    l.foldLeft(0){(x, _) => x + 1}
  }//lengthList

  def reverseList[T](l: List[T]): List[T] =  {
    //5: Find the reverse of a list.
    l.foldRight(List[T]()){(x, y) => y :: x}
  }//reverseList

  def palindrome[T](l: List[T]): Boolean = {
    //6: Check if list is a palindrome
    reverseList(l) == l
  }//palindrome

  def flattenList[T](l: List[List[T]]): List[T] =  {
    //7: Flatten a nested list
    l.foldLeft(List[T]()){(x,xs) => x ::: xs}
  }//flattenList

  def uniqueList[T](l: List[T]): List[T] =  {
    //8: Eliminate duplicate elements of a list
    l.foldRight(List[T]()){ (x, xs) => if (xs.isEmpty || xs.head != xs) x :: xs else xs }
  }//uniqueList

  def packList[T](l: List[T]):List[List[T]] = {
    //9: nest sequential duplicate elements into their own list
    def _packList(run: List[List[T]], l: List[T]):List[List[T]] = l match {
      case Nil => run
      case x :: xs if (run.isEmpty || run.last.head != x) => _packList(run ::: List(List(x)), xs)
      case x :: xs => _packList(run.init ::: List(run.last ::: List(x)), xs)
    }//_packList
    _packList(List(),l)
  }//packList

  def encodeList[T](l: List[T]): List[Tuple2[Int, String]] = {
    //10: Compress list to (no. of elements, element)
    packList(l).map(x => (x.length, x.head))
  }//encodeList

  def encodeListModified[T](l: List[T]): List[Any] = {
    //11: Compress list to (no. of elements, element) if no.of elements >1 else element
    packList(l).map(x => if (x.length==1) x.head else (x.length, x.head))
  }//encodeListModified

  def decodeList[T](l: List[Tuple2[Int, T]]): List[T] = {
    //12: decode a list from 10/11
    l.flatMap(x => List.fill(x._1)(x._2))
  }//decodeList

  def encodeListDirect[T](l: List[T]):List[Tuple2[Int,T]] = {
    //13:encode list directly without using 9
    def _encodeList(run:List[Tuple2[Int,T]], l: List[T]):List[Tuple2[Int,T]] = l match {
      case Nil => run
      case x :: xs if (run.last._2 != x) => _encodeList(run ::: (1,x) :: Nil, xs)
      case x :: xs => _encodeList(run.init ::: (1 + run.last._1, x) :: Nil, xs)
    }//_encodeList
    _encodeList(List((1,l.head)),l.tail)
  }//encodeListDirect

  def duplicateElements[T](l: List[T]): List[T] = {
    //14: Duplcate elements for a list
    l.flatMap(x => List(x, x))
  }//duplicateElements

  def duplicateElementsByN[T](n: Int, l: List[T]): List[T] = {
    //15: Duplicate each element n times
    l.flatMap(x => List[T].fill(n)(x))
  }//duplicateElementsByN

  def dropNthElement[T](n: Int, l: List[T]): List[T] = {
    //16: Drop every N'th element from a list.
    l.zipWithIndex.filter(_._2 % n == 0).map(_._1)
  }//dropNthElement

  def splitAtN[T](n: Int, l: List[T]): (List[T], List[T]) ={
    //17: Split into 2 lists at position n
    def _split[T](n, top: List[T], bottom: List[T]): (List[T], List[T]) = (n, t, b) match {
      case (0, x, xs) => (x, xs)
      case (n, x, xs) => _split(n-1, x :: xs.head, xs.tail)
    }//_split
    _split(n-1, l.head :: Nil, l.tail)
  } //splitAtN

  def extractSlice[T](start: Int, end: Int, l: List[T]): List[T] = {
    //18: return slice of array from start to end values
    l.take(end).drop(start)
  }//extractSlice

  def rotateLeftByN[T](n: Int, l: List[T]) = {
    //19: Shift list n-places to the left
    def _rotate[T](n: Int, l: List[T]): List[T] = (n, l) match {
      case (0, l) => l
      case (n, l) => _rotate(n-1, l.tail :: l.head :: Nil)
    }//_rotate
    _rotate(n-1, l.tail :: l.head :: Nil)
  }//rotateLeftByN

  def removeNthELement[T](l: List[T], n: Int): (List[T], T) = l.splitAt(n) match {
    //20: Remove element at position n
    case (x, i :: xs)  => (x ::: xs, i)
  }//removeNthELement

  def insertAtN[T](l: List[T], p: T, n: Int): List[T] = {
    //21: insert element at index n
    l.zipWithIndex.foldLeft(List[T]()){(acc, x) => if (x._2 == n) acc :: p :: x._1 :: Nil else acc :: x._1 :: Nil}
  }//insertAtN

  def range(lower: Int, upper: Int): List[Int] = {
    //22: create a list with range bounded by lower and upper
    def _range(cur: Int, upper: Int, l: List[Int]): List[Int] = {
      if (cur == upper) l :: upper :: Nil else _range(cur+1, upper, l ::: cur :: Nil)
    }//_range
    _range(lower + 1, upper, lower :: Nil)
  }//range

  def randomNFromList[T](l: List[T], n: Int): List[T] = {
    //23: Draw n random elements from list
    val (xs, run) = removeNthELement(l, (new util.Random).nextInt(l.length))
    run :: randomNFromList(xs, n - 1)
  }//randomNFromList

  def lotto(n: Int, upper: Int): List[Int] = {
    //24: randomly draw n from 1 .. upper
    randomNFromList(List.range(1,upper),n)
  }

  def randomPermutation(l: List[T]): List[T] = {
    //25: randomly shuffle a list
    randomNFromList(l, l.length-1)
  }//randomPermutation

  def permutateList[T](l: List[T], n: Int): List[List[T]] = {
    //26: return all n-permutations of a given list
    l.combinations(n)
  }//permutateList

  def

}//ListProblems
