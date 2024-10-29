! Включаем модуль, который должен быть реализован в первой
! лабораторной работе. Если вы его разместили в ином месте,
! то изменить относительный путь до него.
include "../../lab01/src/calc_array_bounds.f90"
module threads
  use iso_fortran_env, only: int32, int64, real32, real64
  use calc_array_bounds
  implicit none
  include "omp_lib.h"
  private

! Доступные вовне функции и подпрограммы
  public :: test_omp
  
  contains
  
  subroutine test_omp(array, max_threads_num)
    implicit none
    integer(int32), intent(in)  :: max_threads_num
    integer(int32), intent(out), dimension(1:) :: array
    
    integer(int32) :: tn
    integer(int64), dimension(1:2) :: slice, bounds
    
    bounds = [ lbound(array, 1), ubound(array, 1) ]
    
    !$omp parallel num_threads(max_threads_num) private(slice, tn) shared(array, bounds)
      tn = omp_get_thread_num()
      slice = calc_vector_slice(tn, max_threads_num, bounds)
      array(slice(1):slice(2)) = tn
    !$omp end parallel
  end subroutine test_omp
  
end module threads
