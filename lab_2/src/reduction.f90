module reduction
  use iso_fortran_env, only: int32, int64, real32, real64
  implicit none
  private
! Доступные вовне функции и подпрограммы
  public :: omp_sum, omp_max, omp_min
  public :: omp_reduction

  abstract interface
    function omp_reduction(A, ths_num)
      double precision, dimension(1:), intent(in) :: A
      integer, intent(in) :: ths_num
      double precision :: omp_reduction
    end function omp_reduction
  end interface

  contains
  function omp_sum(A, ths_num) result(S)
	implicit none
	real(real64), dimension(1:), intent(in) :: A
	integer(int32), intent(in) :: ths_num
	real(real64) :: S
	integer(int64) :: i
	integer(int64) :: length

	length = size(A)
	S = 0.0d0

	! Параллельный блок с редукцией по сумме
	!$omp parallel do num_threads(ths_num) reduction(+:S)
	do i = 1, length
		S = S + A(i)
	end do
	!$omp end parallel do

  end function omp_sum


  function omp_max(A, ths_num) result(M)
	implicit none
	real(real64), dimension(1:), intent(in) :: A
	integer(int32), intent(in) :: ths_num
	real(real64) :: M
	integer(int64) :: i
	integer(int64) :: length

	length = size(A)
	M = A(1)

	! Параллельный блок с редукцией по максимуму
	!$omp parallel do num_threads(ths_num) reduction(max:M)
	do i = 1, length
		if (A(i) > M) M = A(i)
	end do
	!$omp end parallel do

  end function omp_max


  function omp_min(A, ths_num) result(M)
	implicit none
	real(real64), dimension(1:), intent(in) :: A
	integer(int32), intent(in) :: ths_num
	real(real64) :: M
	integer(int64) :: i
	integer(int64) :: length

	length = size(A)
	M = A(1)

	! Параллельный блок с редукцией по минимуму
	!$omp parallel do num_threads(ths_num) reduction(min:M)
	do i = 1, length
		if (A(i) < M) M = A(i)
	end do
	!$omp end parallel do

  end function omp_min
end module reduction

