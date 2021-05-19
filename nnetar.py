import rpy2
from rpy2 import robjects
from rpy2.robjects.packages import importr
import pandas

path = r"C:\Users\Ilya\OneDrive - Peter the Great St. Petersburg Polytechnical University\project\train_csv.csv"
forecast = importr("forecast")
ts = robjects.r['ts']

robjects.r("""data_solve <- function(data_nms,n){
  return (data_nms[n]);
}""")
check = robjects.globalenv['data_solve']


def readcsv(path):
    """ Чтение документа csv-шника """
    read_csv = robjects.r['read.csv']
    data_for_read = check(read_csv(path), 3)
    return data_for_read


def nnetar(train_data, p, P, size):
    """
    :param train_data: массив для обучения nnetar класса 'ts'
    :param p: Количество несезонных лагов, используемых в качестве входных данных.
    :param P: количество сезонных лагов
    :param size: Количество узлов в скрытом слое
    :return: возвращается обЪект для последующего использования (предсказания данных и   тд.)
    """
    robjects.r("""nnetar_func <- function(train,p_1,P_1,Size){
    return (forecast::nnetar(train,p = p_1,P = P_1,size= Size,maxit=1000,MaxNWts=84581, repeats = 20))
    }""")
    return robjects.globalenv['nnetar_func'](train_data, p, P, size)


def predict_nnetar(fit, j):
    """
    :param fit: объект nnetar
    :param j: количество шагов предсказания
    :return: массив предсказанных данных
    """
    robjects.r("""predict_nnetar <- function(fit,j){
    return (forecast(fit,h = j)$mean)
    }""")
    return robjects.globalenv['predict_nnetar'](fit, j)


def function_for_validation(n, k, data_nms):
    """
    Функция для разбития массива на две части, где одна часть это тренировачные данные, а другая часть это проверочные
    данные.
    :param n: деление массива data_nms на n частей
    :param k: длина(data_nms)/n * k - тренировачные данные, n - длина(data_nms)/n*k - проверочные данные
    :param data_nms: массив для деления на части
    :return: [тренировачные данные, тестовые данные]
    """
    # создание функции для валидации
    robjects.r("""train_data <- function(n,k,data_nms){
    # k - номер теста
    # количество разделений
    data_for_train <- data_nms[1 : round((nrow(data_nms) * 1/n) * k),]
    return (data_for_train)
    }
    test_data <- function(n,k,data_nms){
    # k - номер теста
    # количество разделений
    data_for_test <- data_nms[ (round((nrow(data_nms) * 1/n) * k) + 1) : nrow(data_nms),]
    return (data_for_test)
    }""")
    test_data = robjects.globalenv['test_data']
    train_data = robjects.globalenv['train_data']
    test = test_data(n, k, data_nms)
    train = train_data(n, k, data_nms)
    return (train, test)


def nnetar_validation(data_train,p = 60, P = 6,size = 30,maxit = 1000 ,fit=None,):
    """
    кросс валидация модели 'nnetar' на 'data_train' данных
    :param data_train: данные для обучения модели
    :param fit - модель nnetar (если уже есть обученная модель) (не рекомендуется)
    p, P, size - смотри описание к nnetar
    :return создает вектор моделей, где доступ к ним model[0][0] (первая модель) model[1][0] (вторая модель) (для последующего использования
    воспользуйтесь функцией nnetar_for_vectors) также этот массив содержит информацию о кросс валидации
    """
    robjects.r("""nnetar_validation <- function(train,model,p,P,size, maxit){
    # кросс валидация 
    if(class(model) != "nnetar"){
    return (CVar(train, FUN = nnetar, p = p, P= P, size = size,maxit = maxit,MaxNWts=84581, repeats = 20));
    }else{
    return (CVar(train, FUN = nnetar, model = model));
    }
    }""")
    validation = robjects.globalenv["nnetar_validation"]
    if(fit == None):
        model = validation(data_train,0,p,P,size, maxit)
    else:
        model = validation(data_train,fit,p,P,size, maxit)
    return model

def load_model(path):
    """
    :param path - месторасположение файла с моделью (тип файла .rds)
    :return - возвращает модель
    """
    robjects.r("""load_model <- function(path){
    fit <- readRDS(path)
    return(fit)
    }""")
    load = robjects.globalenv['load_model']
    fit = load(path)
    return fit


def save_model(fit, path):
    """
    Функция сохраняет модель в формате файла .rds
    :param path - месторасположение файла
           """
    robjects.r("""save_model <- function(fit, path){
    saveRDS(fit,path)
    }""")
    save = robjects.globalenv['save_model']
    save(fit, path)
def nnetar_for_predict(fit,data):
    """
    наложение модели fit на данные data (тип данных ts)
    :param fit - модель для использования
    :param data - данные для модели
    :return - возвращается модель для прогнозирования на этих данных
    """
    robjects.r("""
    md <- function(fit,train){
    return(forecast::nnetar(train,model = fit))
    }""")
    return robjects.r["md"](fit,data)

def nnetar_from_vectors(models):
    """ преобразование вектора моделей [r] в список моделей [python] """
    robjects.r("""nnetars <- function(models,i){
    return (models[i]$fold$model)
    }""")
    nnetar_func = robjects.r['nnetars']
    massiv_nnetar = []
    for i in range(1,len(models) - 6):
        massiv_nnetar.append(nnetar_func(models,i))
    return massiv_nnetar
data_for_read = readcsv(path)

data_for_train, data_for_test = function_for_validation(4, 3, data_for_read)

data_for_train = ts(data_for_train, frequency=24)
data_for_test = ts(data_for_test, frequency=24)
