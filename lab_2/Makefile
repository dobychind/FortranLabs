# Компилятор
FC = gfortran
# Параметры компилятора
FFLAGS = -Wall -fopenmp
# Каталог исходных файлов модулей
SRCDIR = ./src
# Каталог для хранения mod файлов
MODDIR = ./mod
# Каталог исполняемых файлов
BDIR = ./bin
# Каталог для тестирующих программ
TESTDIR = ./test
# Временные файлы
CLEAN = *.o *.png *.mod *.zip *.log *~ *.pdf *.dat *.csv *.out
# Получаем полные пути до исходных файлов модулей
SRC = $(wildcard $(SRCDIR)/*.f90)
# Получаем полные пути до исходных файлов тестирующих программ
TEST_SRC = $(wildcard $(TESTDIR)/*.f90)
# Имена исполняемых файлов
TEST = $(addprefix $(BDIR)/, $(notdir $(TEST_SRC:.f90=)))

all: $(TEST)

$(TEST): $(TEST_SRC)

# Правило получения исполняемых файлов
$(BDIR)/%: $(TESTDIR)/%.f90
	@echo "Компилируется файл: \033[1;31m" $< "\033[1;37m"
	@mkdir -p $(MODDIR)
	@mkdir -p $(BDIR)
	$(FC) $(OPENMP) $(FFLAGS) $< -o $@ -J $(MODDIR)

run_test: $(TEST)
	# $(BDIR)/test_threads
	$(BDIR)/test_reduction sum | python plot.py -f png -d imgs/reduction_sum
	$(BDIR)/test_reduction max | python plot.py -f png -d imgs/reduction_max
	$(BDIR)/test_reduction min | python plot.py -f png -d imgs/reduction_min
	$(BDIR)/test_trapezoidal | python plot.py -f png -d imgs/trapezoidal

.PHONY zip:
	zip -r lab02-stud.zip .

.PHONY clean:
	@rm -vf $(CLEAN) $(TEST) $(CALC)
	@rm -vf $(BDIR)/*
	@rm -vf $(MODDIR)/*
	@rm -vf $(SRCDIR)/*.mod $(TESTDIR)/*.mod
	@rm -vfr img/*
	@rm -vfr imgs/*
