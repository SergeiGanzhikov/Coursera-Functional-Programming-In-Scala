def mergeSort[T](list: List[T])(implicit ord: Ordering[T]): List[T] = {
  def merge(leftList: List[T], rightList : List[T]): List[T] =
    (leftList, rightList) match {
      case (Nil, right) => right
      case (left, Nil) => left
      case (leftHead :: leftTail, rightHead:: rightTail) =>
        if (ord.lt(leftHead, rightHead)) leftHead :: merge(leftTail, rightList)
        else rightHead :: merge(leftList, rightTail)
    }

  val mid = list.length / 2
  if (mid == 0) list
  else {
    val (leftHalf, rightHalf) = list splitAt mid
    merge(mergeSort(leftHalf), mergeSort(rightHalf))
  }
}

val nums = List(2, -4, 5, 7, 1)
mergeSort(nums)

val fruits = List("apple", "pineapple", "orange", "banana")
mergeSort(fruits)