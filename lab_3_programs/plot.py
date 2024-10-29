import matplotlib.pyplot as plt
import csv

# Чтение данных из CSV файла
n_people = []
frequency = []

with open('birthday_results.csv', mode='r') as file:
    reader = csv.reader(file)
    next(reader)  # Пропустить заголовок
    for row in reader:
        n_people.append(int(row[0]))        # Преобразуем n к целому
        frequency.append(float(row[1]))    # Преобразуем вероятность к числу с плавающей точкой

# Построение графика
plt.plot(n_people, frequency, marker='o')
plt.title('Вероятность совпадения дней рождения в группе')
plt.xlabel('Количество людей')
plt.ylabel('Вероятность совпадения')
plt.grid(True)
plt.show()
