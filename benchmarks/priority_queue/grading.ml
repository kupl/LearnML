let grading h =
  (findMin h,findMin (deleteMin h))
