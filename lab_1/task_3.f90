program card_probability
    implicit none
    integer, parameter :: trials = 1000000  ! Количество испытаний (1 миллион)
    integer :: i, ace_count_A = 0, ace_count_AB = 0  ! Счетчики для событий A и AB
    integer :: first_card, second_card  ! Переменные для индексов первой и второй карты
    character(len=6), dimension(36) :: deck, shuffled_deck  ! Массивы для хранения карт и перемешанной колоды

    ! Определим колоду карт (36 карт)
    deck = [ &  ! Колода состоит из 36 карт, каждая карта имеет символическую длину 6 (чтобы учесть масть и номинал)
    "T♣   ", "K♣   ", "D♣   ", "V♣   ", "6♣   ", "7♣   ", "8♣   ", "9♣   ", "10♣  ", &  ! Трефы
    "T♠   ", "K♠   ", "D♠   ", "V♠   ", "6♠   ", "7♠   ", "8♠   ", "9♠   ", "10♠  ", &  ! Пики
    "T♦   ", "K♦   ", "D♦   ", "V♦   ", "6♦   ", "7♦   ", "8♦   ", "9♦   ", "10♦  ", &  ! Бубны
    "T♥   ", "K♥   ", "D♥   ", "V♥   ", "6♥   ", "7♥   ", "8♥   ", "9♥   ", "10♥  " ]  ! Червы

    ! Повторим испытание trials раз
    do i = 1, trials
        shuffled_deck = deck  ! Создаем копию колоды для каждого испытания, чтобы оригинальная колода не изменилась

        call shuffle_deck(shuffled_deck)  ! Перемешиваем колоду с помощью подпрограммы shuffle_deck

        ! Выберем две карты
        first_card = 1  ! Первая карта (индекс первой карты)
        second_card = 2  ! Вторая карта (индекс второй карты)

        ! Проверяем, является ли вторая карта тузом
        if (index(shuffled_deck(second_card), "T") == 1) then  ! Если вторая карта содержит символ "T" (туз)
            ace_count_A = ace_count_A + 1  ! Увеличиваем счетчик для события A (вторая карта — туз)
        end if

        ! Проверяем условную вероятность: если первая карта — туз и вторая карта тоже туз
        if (index(shuffled_deck(first_card), "T") == 1 .and. index(shuffled_deck(second_card), "T") == 1) then
            ace_count_AB = ace_count_AB + 1  ! Увеличиваем счетчик для события AB (первая карта туз и вторая карта туз)
        end if
    end do

    ! Выводим результаты
    print *, "Безусловная вероятность P(A) того, что вторая карта туз: "
    print '(F10.6)', real(ace_count_A) / real(trials)
    ! Выводим безусловную вероятность того, что вторая карта — туз (частота события A)

    print *, "Условная вероятность P(A|B), что вторая карта туз," // &
             " если первая карта тоже туз (AB / A):"
    if (ace_count_A > 0) then
        print '(F10.6)', real(ace_count_AB) / real(ace_count_A)
    else
        print *, "Условная вероятность не может быть рассчитана, так как " & 
               // "событие A не произошло."
    end if

contains

    ! Процедура перемешивания колоды карт (алгоритм Fisher-Yates)
    subroutine shuffle_deck(deck)
        implicit none
        character(len=*), dimension(:), intent(inout) :: deck  ! Массив карт для перемешивания (входной и изменяемый параметр)
        character(len=6) :: temp  ! Временная переменная для хранения карты
        real :: r  ! Переменная для случайного числа
        integer :: i, j, n  ! Переменные для индексов и размера колоды
        n = size(deck)  ! Получаем количество карт в колоде
        
        ! Fisher-Yates перемешивание
        do i = n, 2, -1  ! Идем с последней карты к первой
            call random_number(r)  ! Генерируем случайное число от 0 до 1
            j = int(r * i) + 1  ! Выбираем случайный индекс от 1 до i
            temp = deck(i)  ! Меняем местами карты deck(i) и deck(j)
            deck(i) = deck(j)
            deck(j) = temp
        end do
    end subroutine shuffle_deck

end program card_probability
