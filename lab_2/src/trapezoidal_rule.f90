module trapezoidal_rule
  use iso_fortran_env, only : int32, int64, real32, real64
  use omp_lib
  implicit none

  abstract interface
    ! Абстрактный интерфейс для подынтегральной функции
    pure function f(x)
      double precision, intent(in) :: x
      double precision :: f
    end function f
  end interface

  private

  public :: trapezoidal

  contains
  !----------------------------------------------------------------
  ! Функция вычисляющая значение интеграла по формуле трапеции
  ! func --- подынтегральная функция от одного аргумента
  ! a и b задают пределы интегрирования
  ! n --- число точек разбиения отрезка [a, b]
  ! threads_num --- число потоков для распараллеливания
  !----------------------------------------------------------------
  function trapezoidal(func, a, b, n, threads_num) result (res)
    implicit none
    procedure(f) :: func
    real(real64), intent(in) :: a, b
    integer(int64), intent(in) :: n
    integer(int64), intent(in) :: threads_num
    real(real64) :: res

    real(real64) :: h, x_i, sum
    integer(int64) :: i

    ! Шаг интегрирования
    h = (b - a) / real(n, kind=real64)

    ! Начальная сумма: трапециевидное приближение краев
    res = (func(a) + func(b)) * h / 2.0

    sum = 0.0

    ! Параллельный блок с редукцией для вычисления суммы значений f(x_i)
    !$omp parallel do private(x_i) reduction(+:sum) num_threads(threads_num)
    do i = 1, n-1
      x_i = a + i * h
      sum = sum + func(x_i)
    end do
    !$omp end parallel do

    ! Добавляем основную сумму
    res = res + sum * h

  end function trapezoidal

end module trapezoidal_rule
