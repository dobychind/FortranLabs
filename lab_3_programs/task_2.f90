program monty_hall  									! Начало программы моделирования задачи Монти Холла методом Монте-Карло
    use omp_lib  									! Подключение библиотеки OpenMP для многопоточного программирования
    implicit none  									! Отключение неявного объявления переменных

    integer, parameter :: num_trials = 1e6  						! Количество испытаний (миллион раз)
    integer :: win_no_change = 0, win_change = 0  					! Счётчики побед для стратегий: без смены выбора и со сменой
    integer :: i, prize_door, player_choice, revealed_door, change_choice  		! Переменные для логики игры
    real :: rnum  ! Переменная для генерации случайных чисел (0 <= rnum < 1)
    integer :: seed_size  ! Размер массива для генератора случайных чисел
    integer, dimension(:), allocatable :: seed  					! Массив для инициализации генератора случайных чисел

    ! Узнаем размер массива для seed
    call random_seed(size=seed_size)  							! Вызов функции для получения размера массива seed
    allocate(seed(seed_size))  								! Динамическое выделение памяти для массива seed
    
    ! Инициализация генератора случайных чисел
    seed = [(i, i = 1, seed_size)]  							! Заполняем массив seed значениями от 1 до seed_size
    call random_seed(put=seed)  							! Устанавливаем начальное состояние (seed) для генератора случайных чисел

    ! Параллельная секция OpenMP для многопоточной обработки
    !$omp parallel private(i, prize_door, player_choice, revealed_door, change_choice, rnum) shared(win_no_change, win_change)
    !$omp do reduction(+:win_no_change, win_change)  					! Выполняем параллельный цикл с накоплением значений в переменных win_no_change и win_change
    do i = 1, num_trials  								! Цикл для проведения одного миллиона игр

        ! 1. Случайный выбор двери с машиной
        call random_number(rnum)  							! Генерация случайного числа
        prize_door = int(rnum * 3) + 1  						! Машина за дверью 1, 2 или 3

        ! 2. Игрок делает случайный выбор двери
        call random_number(rnum)  							! Генерация случайного числа
        player_choice = int(rnum * 3) + 1  						! Игрок случайно выбирает дверь 1, 2 или 3

        ! 3. Ведущий открывает одну из оставшихся дверей, за которой козёл
        do
            call random_number(rnum)  ! Генерация случайного числа
            revealed_door = int(rnum * 3) + 1  ! Ведущий случайно выбирает дверь
            if (revealed_door /= prize_door .and. revealed_door /= player_choice) exit  ! Ведущий открывает дверь с козлом, которая не является дверью с машиной и не выбрана игроком
        end do

        ! 4. Стратегия 1: игрок не меняет свой выбор
        if (player_choice == prize_door) then  						! Если игрок изначально выбрал дверь с машиной
            win_no_change = win_no_change + 1  						! Победа для стратегии "не менять выбор"
        end if

        ! 5. Стратегия 2: игрок меняет свой выбор
        do
            call random_number(rnum)  							! Генерация случайного числа
            change_choice = int(rnum * 3) + 1  						! Игрок выбирает другую дверь
            if (change_choice /= player_choice .and. change_choice /= revealed_door) exit  ! Игрок выбирает дверь, которая не была выбрана ранее и не открыта ведущим
        end do

        if (change_choice == prize_door) then  						! Если после смены выбора игрок выбрал дверь с машиной
            win_change = win_change + 1  						! Победа для стратегии "сменить выбор"
        end if
    end do
    !$omp end do  									! Завершение параллельного цикла
    !$omp end parallel  								! Завершение параллельной секции OpenMP

    ! Вывод результатов
    print *, "Количество испытаний: ", num_trials  					! Выводим общее количество испытаний
    print *, "Побед при стратегии без смены выбора: ", win_no_change  			! Сколько раз победила стратегия "не менять выбор"
    print *, "Частота побед при стратегии без смены выбора: ", real(win_no_change) / num_trials  		! Вероятность победы для стратегии "не менять выбор"
    print *, "Побед при стратегии со сменой выбора: ", win_change  						! Сколько раз победила стратегия "сменить выбор"
    print *, "Частота побед при стратегии со сменой выбора: ", real(win_change) / num_trials  			! Вероятность победы для стратегии "сменить выбор"

    deallocate(seed)  									! Освобождаем память, выделенную под массив seed

end program monty_hall  								! Конец программы
