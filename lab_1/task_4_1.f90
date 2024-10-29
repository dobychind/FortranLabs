program vectorize
  implicit none
  real, dimension(7) :: a, b  

  ! Инициализация массивов
  a = [2.0, 3.0, 5.0, 7.0, 11.0, 13.0, 17.0]
  b = [4.0, 6.0, 8.0, 10.0, 12.0, 14.0, 16.0]

  ! Вывод суммы произведения элементов массивов
  print *, sum(a*b)

  ! Вывод результата через встроенную функцию dot_product
  print *, dot_product(a, b)
end program vectorize
