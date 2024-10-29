program birthday_paradox
    use omp_lib   									! Подключаем библиотеку OpenMP для параллельных вычислений
    implicit none

    ! Параметры программы
    integer, parameter :: num_trials = 1000000  					! Количество испытаний (экспериментов)
    integer, parameter :: max_people = 100      					! Максимальное количество людей в группе
    integer, parameter :: days_in_year = 365    					! Количество дней в году (вместо 365 можно было бы задать любое значение)

    ! Объявление переменных
    integer :: n, i, j, k, trials_with_match    					! n — количество людей, i — индекс испытания, j, k — индексы для проверки совпадений
    logical :: has_match                        					! Логическая переменная для проверки, есть ли совпадение в текущем испытании
    integer :: birthdays(max_people)            					! Массив для хранения случайных дней рождения участников группы
    integer :: count_matches(max_people)        					! Массив для подсчета совпадений для каждой группы с разным числом участников
    real :: frequency(max_people)               					! Массив для хранения частоты совпадений для каждой группы
    real :: rnum                                					! Переменная для хранения случайного числа
    character(len=20) :: filename               					! Переменная для хранения имени файла
    integer :: unit_num                         					! Номер для работы с файлом

    ! Инициализация начальных значений
    count_matches = 0  									! Устанавливаем количество совпадений для каждого количества людей в 0

    ! Параллельная секция для проведения экспериментов
    !$omp parallel private(i, j, k, birthdays, has_match, rnum) shared(count_matches)
    do n = 2, max_people  								! Цикл по количеству людей в группе от 2 до max_people (100)
        !$omp do reduction(+:count_matches)
        do i = 1, num_trials  								! Проводим num_trials (1 миллион) испытаний для каждой группы из n человек
            birthdays = 0  								! Обнуляем массив с днями рождения для нового эксперимента
            has_match = .false.  							! Сбрасываем флаг совпадения дней рождения

            ! Генерация случайных дней рождения для группы из n человек
            do j = 1, n
                call random_number(rnum)  						! Генерируем случайное число от 0 до 1
                birthdays(j) = int(rnum * days_in_year) + 1  				! Преобразуем случайное число в день от 1 до 365
            end do

            ! Проверка на совпадения
            do j = 1, n - 1  								! Перебираем людей в группе
                do k = j + 1, n  							! Сравниваем каждого с последующими
                    if (birthdays(j) == birthdays(k)) then  				! Если дни рождения совпадают
                        has_match = .true.  						! Нашли совпадение
                        exit  								! Прерываем внутренний цикл
                    end if
                end do
                if (has_match) exit  							! Прерываем внешний цикл, если уже есть совпадение
            end do

            ! Если было найдено совпадение, увеличиваем счетчик совпадений для текущего n
            if (has_match) count_matches(n) = count_matches(n) + 1
        end do
        !$omp end do
    end do
    !$omp end parallel

    ! Открываем файл для записи данных
    filename = 'birthday_results.csv'  							! Имя файла для сохранения результатов
    unit_num = 10  									! Номер файла для открытия
    open(unit=unit_num, file=filename, status='replace')  				! Открываем файл на запись с заменой содержимого

    ! Записываем заголовки в CSV файл
    write(unit_num, *) 'n,frequency'  							! Записываем первую строку: n (количество людей), frequency (частота совпадений)

    ! Вычисление частоты совпадений для каждого количества людей и запись данных в файл
    do n = 2, max_people
        frequency(n) = real(count_matches(n)) / num_trials  				! Вычисляем частоту совпадений как отношение числа совпадений к количеству испытаний
        print *, "Количество людей: ", n, " Частота совпадений: ", frequency(n)  	! Выводим результаты в консоль
        write(unit_num, '(I3, ",", F7.5)') n, frequency(n)  				! Записываем количество людей и частоту совпадений в файл в формате CSV
    end do

    ! Закрываем файл
    close(unit_num)  									! Закрываем файл после завершения записи

end program birthday_paradox
