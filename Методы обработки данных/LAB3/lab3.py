from collections import defaultdict
import math
from sklearn.model_selection import train_test_split

class NaiveBayesSpamFilter:
    def __init__(self):
        self.spam_words = defaultdict(int)
        self.ham_words = defaultdict(int)
        self.spam_count = 0
        self.ham_count = 0
        self.vocab = set()

    def train(self, dataset):
        """
        dataset = [(text, label), ...] где label = 'spam' или 'ham'
        """
        for text, label in dataset:
            words = text.lower().split()
            self.vocab.update(words)
            if label == "spam":
                self.spam_count += 1
                for w in words:
                    self.spam_words[w] += 1
            else:
                self.ham_count += 1
                for w in words:
                    self.ham_words[w] += 1

    def predict(self, text):
        words = text.lower().split()
        total = self.spam_count + self.ham_count
        p_spam = math.log(self.spam_count / total)
        p_ham = math.log(self.ham_count / total)

        for w in words:
            # Сглаживание Лапласа
            p_spam += math.log((self.spam_words[w] + 1) /
                               (sum(self.spam_words.values()) + len(self.vocab)))
            p_ham += math.log((self.ham_words[w] + 1) /
                              (sum(self.ham_words.values()) + len(self.vocab)))

        return "spam" if p_spam > p_ham else "ham"



def load_DataSet(path: str)-> list:
    dataset = []
    with open(path, 'r', encoding="utf-8") as f:
        for line in f:
            parts = line.strip().split('\t', 1)
            if len(parts) == 2:
                label, text = parts
                dataset.append((text, label))
    return dataset





if __name__ == "__main__":

    # Загрузка данных
    train_data = load_DataSet("spam_collection/SMSSpamCollection")
    
    
    # Разбивка на обучающую и тестовую выборки
    train_dataset, test_dataset = train_test_split(train_data, test_size=0.2, random_state=42)
    
    print(f"Всего сообщений: {len(train_data)}")
    print(f"Обучающая выборка: {len(train_dataset)}")
    print(f"Тестовая выборка: {len(test_dataset)}")

    # Примеры из обучающей выборки
    print("\nПримеры из обучающей выборки:")
    for i in range(5):
        print(train_dataset[i])
    
    # Обучение фильтра
    filter = NaiveBayesSpamFilter()
    filter.train(train_dataset)

    # Проверка на тестовой выборке
    correct = 0
    for text, label in test_dataset:
        prediction = filter.predict(text)
        if prediction == label:
            correct += 1

    accuracy = correct / len(test_dataset)
    print(f"\nТочность фильтра: {accuracy:.2%}")
    
    
    
    # from sklearn.metrics import confusion_matrix, ConfusionMatrixDisplay
    import matplotlib.pyplot as plt

    # # Получение предсказаний для тестовой выборки
    # y_true = [label for _, label in test_dataset]
    # y_pred = [filter.predict(text) for text, _ in test_dataset]

    # # Построение матрицы ошибок
    # cm = confusion_matrix(y_true, y_pred, labels=["ham", "spam"])
    # disp = ConfusionMatrixDisplay(confusion_matrix=cm, display_labels=["ham", "spam"])
    # disp.plot(cmap=plt.cm.Blues)
    # plt.title("Матрица ошибок")
    # plt.show()
    
    
    
    
    #####################################
    import seaborn as sns

    # # Подготовка данных
    # lengths = {"spam": [], "ham": []}
    # for text, label in train_data:
    #     lengths[label].append(len(text.split()))

    # # Построение гистограммы
    # plt.figure(figsize=(10, 6))
    # sns.histplot(data=lengths, kde=True, bins=30, alpha=0.5)
    # plt.title("Распределение длины сообщений")
    # plt.xlabel("Количество слов")
    # plt.ylabel("Частота")
    # plt.legend(title="Класс")
    # plt.show()
    ###################################
    
    # import pandas as pd

    # # Получение топ-N слов для каждого класса
    # def get_top_words(word_counts, n=10):
    #     return sorted(word_counts.items(), key=lambda x: x[1], reverse=True)[:n]

    # top_spam = get_top_words(filter.spam_words)
    # top_ham = get_top_words(filter.ham_words)

    # # Визуализация
    # df_spam = pd.DataFrame(top_spam, columns=["Слово", "Частота"])
    # df_ham = pd.DataFrame(top_ham, columns=["Слово", "Частота"])

    # plt.figure(figsize=(12, 6))
    # plt.subplot(1, 2, 1)
    # sns.barplot(x="Частота", y="Слово", data=df_spam)
    # plt.title("Топ-10 слов в spam")

    # plt.subplot(1, 2, 2)
    # sns.barplot(x="Частота", y="Слово", data=df_ham)
    # plt.title("Топ-10 слов в ham")

    # plt.tight_layout()
    # plt.show()
        
    