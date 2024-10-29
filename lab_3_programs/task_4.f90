program gambler_ruin
    use omp_lib  								! Подключаем библиотеку OpenMP для использования параллельных вычислений
    implicit none  								! Требование явного объявления всех переменных

    ! Объявление параметров (констант)
    integer, parameter :: N = 100            					! Общий капитал (N рублей), максимальное количество рублей в системе
    integer, parameter :: num_trials = 1000000  				! Количество игр (испытаний), которые будут проведены

    ! Объявление переменных
    integer :: n_initial, current_capital, i, round_count, wins, losses
    										! n_initial - начальный капитал игрока №1
    										! current_capital - текущий капитал игрока №1 в ходе игры
    										! i - переменная для цикла, индекс испытаний
    										! round_count - количество раундов в одной игре
    										! wins - количество побед игрока №1
    										! losses - количество разорений игрока №1

    real :: p, q, rnum  							! p - вероятность выигрыша игрока №1, q - вероятность проигрыша, rnum - случайное число
    real :: avg_rounds, total_rounds  						! avg_rounds - среднее количество раундов, total_rounds - общее количество раундов за все игры

    ! Инициализация переменных
    n_initial = 90  								! Начальный капитал игрока №1 (90 рублей)
    p = 0.5         								! Вероятность выигрыша игрока №1 (0.5)
    q = 1.0 - p     								! Вероятность проигрыша игрока №1 (0.5)
    wins = 0        								! Начальное значение количества побед игрока №1 - 0
    losses = 0      								! Начальное значение количества разорений игрока №1 - 0
    total_rounds = 0  								! Изначально общее количество раундов равно 0

    ! Параллельное выполнение Монте-Карло методом (имитация игр)
    !$omp parallel private(current_capital, round_count, i, rnum) shared(wins, losses, total_rounds)
    ! Объявление параллельного региона OpenMP, с разделением переменных: 
    ! current_capital, round_count, i, rnum будут уникальны для каждой нити (private),
    ! а wins, losses, total_rounds будут общими для всех (shared)

    !$omp do reduction(+:wins, losses, total_rounds)
    ! Цикл с директивой OpenMP. Редукция (reduction) суммирует переменные wins, losses и total_rounds параллельно
    do i = 1, num_trials  							! Цикл по количеству игр (num_trials)

        current_capital = n_initial  						! Устанавливаем начальный капитал для игрока №1
        round_count = 0  							! Обнуляем счетчик раундов для новой игры

        ! Цикл игры (случайное блуждание)
        do while (current_capital > 0 .and. current_capital < N)
            call random_number(rnum)  						! Генерация случайного числа от 0 до 1
            if (rnum < p) then
                current_capital = current_capital + 1  				! Игрок выиграл раунд, его капитал увеличился на 1 рубль
            else
                current_capital = current_capital - 1  				! Игрок проиграл раунд, его капитал уменьшился на 1 рубль
            end if
            round_count = round_count + 1  					! Увеличиваем количество раундов
        end do

        ! Проверка исхода игры
        if (current_capital == N) then
            wins = wins + 1  							! Если капитал достиг N рублей, то игрок №1 выиграл игру
        else
            losses = losses + 1  						! Если капитал упал до 0 рублей, игрок №1 разорился
        end if

        total_rounds = total_rounds + round_count  				! Суммируем количество раундов для статистики
    end do
    !$omp end do  								! Закрываем параллельный цикл OpenMP
    !$omp end parallel  							! Закрываем параллельный регион OpenMP

    ! Вычисление среднего числа раундов на одну игру
    avg_rounds = real(total_rounds) / num_trials  				! Преобразуем total_rounds в тип real для деления

    ! Вывод результатов на экран
    print *, "Количество игр: ", num_trials  					! Выводим общее количество игр
    print *, "Количество побед игрока 1: ", wins  				! Выводим количество побед игрока №1
    print *, "Количество разорений игрока 1: ", losses  			! Выводим количество разорений игрока №1
    print *, "Вероятность победы игрока 1: ", real(wins) / num_trials  		! Выводим вероятность победы игрока №1
    print *, "Среднее количество раундов: ", avg_rounds  			! Выводим среднее количество раундов на игру

end program gambler_ruin  							! Конец программы
