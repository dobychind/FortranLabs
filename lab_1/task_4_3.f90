program vectorize
    implicit none
    real, dimension(7) :: a, b
    real :: result
    integer :: i

    ! Инициализация массивов
    a = [2.0, 3.0, 5.0, 7.0, 11.0, 13.0, 17.0]
    b = [4.0, 6.0, 8.0, 10.0, 12.0, 14.0, 16.0]
    
    ! Вычисление скалярного произведения через цикл
    result = 0.0
    do i = 1, 7
        result = result + a(i) * b(i)
    end do

    ! Вывод результата
    print *, "Скалярное произведение через цикл: ", result
end program vectorize