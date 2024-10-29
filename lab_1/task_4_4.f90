program vectorize_dynamic
  implicit none
  integer :: n
  real, allocatable :: a(:), b(:)
  real :: result, dot_result
  integer :: i

  ! Задайте размерность массива
  n = 7
  allocate(a(n), b(n))

  ! Инициализация массивов
  a = [2.0, 3.0, 5.0, 7.0, 11.0, 13.0, 17.0]
  b = [4.0, 6.0, 8.0, 10.0, 12.0, 14.0, 16.0]

  ! Скалярное произведение с использованием суммирования в цикле
  result = 0.0
  do i = 1, n
      result = result + a(i) * b(i)
  end do

  print *, "Скалярное произведение через цикл:", result

  ! Скалярное произведение с использованием dot_product
  dot_result = dot_product(a, b)
  print *, "Скалярное произведение с dot_product:", dot_result

  ! Освобождение памяти
  deallocate(a, b)
end program vectorize_dynamic