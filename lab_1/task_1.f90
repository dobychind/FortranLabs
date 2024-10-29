program queue_simulation
    use iso_fortran_env  									! Используем стандарт ISO, чтобы задать точные типы данных (например, int64)
    implicit none  										! Явное объявление всех переменных обязательно

    ! Параметры программы
    integer(int64), parameter :: n = 1e3  							! Константа: количество покупателей с рублями (итого в очереди 2n)
    integer(int8), allocatable :: Q(:)    							! Динамический массив для очереди (int8 для экономии памяти)
    integer(int64) :: i, success_count, total_simulations  					! Счетчики и параметры
    real :: probability  									! Переменная для хранения вычисленной вероятности

    ! Параметры моделирования
    total_simulations = 10000  									! Количество симуляций

    ! Инициализация динамического массива Q для очереди
    if (.not. allocated(Q)) allocate(Q(2*n))  							! Если массив не выделен, выделяем память на 2n элементов
    
    ! Основной цикл по симуляциям
    success_count = 0  										! Инициализируем счетчик успешных симуляций
    do i = 1, total_simulations
        call fill_queue(Q, n)  									! Заполняем очередь покупателями (половина с рублями, половина с полтинниками)
        call shuffle_queue(Q)  									! Перемешиваем очередь случайным образом
        if (check_trajectory(Q)) then  								! Проверяем, ни разу ли сумма не стала отрицательной
            success_count = success_count + 1  							! Если траектория успешная, увеличиваем счетчик
        end if
    end do
    
    ! Вычисление вероятности как доли успешных симуляций
    probability = real(success_count) / real(total_simulations)
    print '(A, F10.6)', 'Вероятность того, что никто не будет ждать сдачу: ', probability

contains

    ! Подпрограмма для заполнения очереди
    subroutine fill_queue(Q, n)
        implicit none
        integer(int8), dimension(:), intent(out) :: Q  						! Массив очереди передается по ссылке
        integer(int64), intent(in) :: n  							! Количество покупателей с рублями (и полтинниками)
        integer(int64) :: i  									! Счетчик

        ! Заполняем первую половину массива покупателями с рублями, вторую — покупателями с полтинниками
        do i = 1, n
            Q(i) = 1_int8     									! Покупатели с рублями получают значение +1
            Q(n + i) = -1_int8  								! Покупатели с 50 копейками получают значение -1
        end do
    end subroutine fill_queue

    ! Подпрограмма для перемешивания очереди
    subroutine shuffle_queue(Q)
        implicit none
        integer(int8), dimension(:), intent(inout) :: Q  					! Массив очереди передается по ссылке для изменения
        integer(int8) :: temp  									! Временная переменная для обмена элементов
        real :: u  										! Переменная для генерации случайного числа
        integer :: i, j, m  									! Счетчики и размер массива

        m = size(Q)  ! Определяем размер массива Q
        do i = 1, m
            call random_number(u)  								! Генерация случайного числа от 0 до 1
            j = int(u * m) + 1  								! Преобразуем случайное число в индекс от 1 до m
            ! Обмен элементов Q(i) и Q(j)
            temp = Q(i)
            Q(i) = Q(j)
            Q(j) = temp
        end do
    end subroutine shuffle_queue

    ! Функция для проверки траектории
    logical function check_trajectory(Q)
        implicit none
        integer(int8), dimension(:), intent(in) :: Q  						! Массив очереди передается по ссылке для анализа
        integer(int64) :: i, sum  								! Счетчики и переменная для суммы значений в очереди

        sum = 0  ! Инициализируем сумму
        check_trajectory = .true.  								! Предполагаем, что траектория успешна
        do i = 1, size(Q)
            sum = sum + Q(i)  									! Накапливаем сумму: +1 для рубля, -1 для полтинника
            if (sum < 0) then  									! Если сумма в любой момент становится отрицательной
                check_trajectory = .false.  							! То кто-то ждет сдачу, траектория неудачна
                exit  										! Прерываем цикл, дальнейшая проверка не нужна
            end if
        end do
    end function check_trajectory

end program queue_simulation