def mergeSort(list: List[Int]): List[Int] = {
  val mid = list.length / 2
  if (mid == 0) list
  else {
    val (leftHalf, rightHalf) = list splitAt mid
    merge(mergeSort(leftHalf), mergeSort(rightHalf))
  }
}

def merge(leftList: List[Int], rightList : List[Int]): List[Int] =
  (leftList, rightList) match {
    case (Nil, right) => right
    case (left, Nil) => left
    case (leftHead :: leftTail, rightHead:: rightTail) =>
      if (leftHead < rightHead) leftHead :: merge(leftTail, rightList)
      else rightHead :: merge(leftList, rightTail)
}

val nums = List(2, -4, 5, 7, 1)
mergeSort(nums)