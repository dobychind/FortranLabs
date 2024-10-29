program token_game  										! Программа моделирует игру с жетонами между тремя игроками
    use omp_lib  										! Подключение библиотеки OpenMP для многопоточной обработки
    implicit none  										! Отключение неявного объявления типов переменных

    integer, parameter :: num_trials = 1000000  						! Количество испытаний (миллион игр)
    integer :: x, y, z  									! Текущее количество жетонов у игроков 1, 2 и 3
    integer :: initial_x, initial_y, initial_z  						! Начальное количество жетонов у игроков
    integer :: round_count, trial  								! Счетчик раундов и переменная для цикла испытаний
    integer :: winner, total_rounds  								! Переменная для хранения победителя и общего числа раундов
    integer :: wins_player1, wins_player2, wins_player3  					! Счетчики побед для каждого игрока
    real :: rnum  										! Переменная для генерации случайных чисел
    real :: theoretical_duration  								! Теоретическое значение средней продолжительности игры

    ! Инициализация начальных жетонов для каждого игрока
    initial_x = 5 							 			! Игрок 1 начинает с 5 жетонами
    initial_y = 5  										! Игрок 2 начинает с 5 жетонами
    initial_z = 5  										! Игрок 3 начинает с 5 жетонами

    ! Инициализация счетчиков побед и общего количества раундов
    wins_player1 = 0  										! Счетчик побед игрока 1
    wins_player2 = 0  										! Счетчик побед игрока 2
    wins_player3 = 0  										! Счетчик побед игрока 3
    total_rounds = 0  										! Общий счетчик раундов

    ! Параллельный блок с использованием OpenMP
    !$omp parallel private(x, y, z, round_count, trial, winner, rnum) shared(wins_player1, wins_player2, wins_player3, total_rounds)
    !$omp do reduction(+:wins_player1, wins_player2, wins_player3, total_rounds)
    do trial = 1, num_trials  									! Цикл для одного миллиона игр

        ! Инициализация жетонов перед каждой игрой
        x = initial_x  										! Количество жетонов игрока 1
        y = initial_y  										! Количество жетонов игрока 2
        z = initial_z  										! Количество жетонов игрока 3
        round_count = 0  									! Счетчик раундов для текущей игры

        ! Игра продолжается, пока у каждого игрока есть хотя бы один жетон
        do while (x > 0 .and. y > 0 .and. z > 0)
            call random_number(rnum)  								! Генерация случайного числа
            winner = int(3 * rnum) + 1  							! Случайный выбор победителя раунда (значение от 1 до 3)
	
            ! Каждый игрок ставит на кон по одному жетону
            x = x - 1  										! Игрок 1 теряет 1 жетон
            y = y - 1  										! Игрок 2 теряет 1 жетон
            z = z - 1  										! Игрок 3 теряет 1 жетон

            ! Победитель раунда забирает все три жетона
            if (winner == 1) then  								! Если победил игрок 1
                x = x + 3  									! Игрок 1 получает три жетона
            elseif (winner == 2) then  								! Если победил игрок 2
                y = y + 3  									! Игрок 2 получает три жетона
            elseif (winner == 3) then  								! Если победил игрок 3
                z = z + 3  									! Игрок 3 получает три жетона
            end if

            round_count = round_count + 1  							! Увеличиваем счетчик раундов
        end do

        ! Определяем победителя игры
        if (x > 0) then  									! Если у игрока 1 остались жетоны, он победил
            wins_player1 = wins_player1 + 1  							! Увеличиваем счетчик побед игрока 1
        elseif (y > 0) then  									! Если у игрока 2 остались жетоны, он победил
            wins_player2 = wins_player2 + 1  							! Увеличиваем счетчик побед игрока 2
        elseif (z > 0) then  									! Если у игрока 3 остались жетоны, он победил
            wins_player3 = wins_player3 + 1  							! Увеличиваем счетчик побед игрока 3
        end if

        ! Суммируем количество раундов для всех игр
        total_rounds = total_rounds + round_count  						! Добавляем количество раундов этой игры к общему числу
    end do
    !$omp end do  										! Завершение параллельного цикла
    !$omp end parallel  									! Завершение параллельного блока

    ! Вычисление теоретического значения средней продолжительности игры
    theoretical_duration = real(initial_x * initial_y * initial_z) / real(initial_x + initial_y + initial_z - 2)

    ! Вывод результатов моделирования
    print *, "Частота побед игрока 1: ", real(wins_player1) / real(num_trials)  		! Частота побед игрока 1
    print *, "Частота побед игрока 2: ", real(wins_player2) / real(num_trials)  		! Частота побед игрока 2
    print *, "Частота побед игрока 3: ", real(wins_player3) / real(num_trials)  		! Частота побед игрока 3
    print *, "Среднее количество раундов: ", real(total_rounds) / real(num_trials)  		! Среднее количество раундов за игру
    print *, "Теоретическое среднее количество раундов: ", theoretical_duration  		! Теоретическое значение среднего количества раундов

end program token_game  									! Конец программы