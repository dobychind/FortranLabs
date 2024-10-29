# Компилятор
FC = gfortran

# Параметры компилятора
FFLAGS = -Wall -fopenmp

# Директории
SRCDIR = ./src
MODDIR = ./mod
BDIR = ./bin
TESTDIR = ./test
IMGDIR = ./imgs

# Временные файлы
CLEAN = *.o *.png *.mod *.zip *.log *~ *.pdf *.dat *.csv *.out

# Получаем полные пути до исходных файлов модулей
SRC = $(wildcard $(SRCDIR)/*.f90)

# Получаем полные пути до исходных файлов тестирующих программ
TEST_SRC = $(wildcard $(TESTDIR)/*.f90)

# Имена исполняемых файлов
TEST = $(addprefix $(BDIR)/, $(notdir $(TEST_SRC:.f90=)))

# Установка значений по умолчанию
DEFAULT_THREADS = 2

.PHONY: all clean zip run_test

# Основная цель
all: $(TEST)

$(TEST): $(TEST_SRC)

# Правило получения исполняемых файлов
$(BDIR)/%: $(TESTDIR)/%.f90
	@echo "Компилируется файл: \033[1;31m" $< "\033[1;37m"
	@mkdir -p $(MODDIR)
	@mkdir -p $(BDIR)
	$(FC) $(FFLAGS) $< -o $@ -J $(MODDIR)

run_test: $(TEST)
	@echo "Запуск тестов с количеством потоков: $(DEFAULT_THREADS)"
	$(BDIR)/test_reduction sum | python3 plot.py -f png -d $(IMGDIR)/reduction_sum --threads=$(DEFAULT_THREADS)
	$(BDIR)/test_reduction max | python3 plot.py -f png -d $(IMGDIR)/reduction_max --threads=$(DEFAULT_THREADS)
	$(BDIR)/test_reduction min | python3 plot.py -f png -d $(IMGDIR)/reduction_min --threads=$(DEFAULT_THREADS)
	$(BDIR)/test_trapezoidal | python3 plot.py -f png -d $(IMGDIR)/trapezoidal --threads=$(DEFAULT_THREADS)

# Упаковка файлов в zip
zip:
	zip -r lab02-stud.zip .

# Очистка временных файлов
clean:
	@rm -vf $(CLEAN) $(TEST) $(BDIR)/*
	@rm -vf $(MODDIR)/*
	@rm -vf $(SRCDIR)/*.mod $(TESTDIR)/*.mod
	@rm -vfr $(IMGDIR)/*
