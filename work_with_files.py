import pandas as pd
import plotly.graph_objs as go
from functools import reduce
import os


def diff_plot_data(time_days, **info):
    """ График дифф табличной фукции time_days. left и right - просматриваемые пункты (от и до) - тип int"""
    try:
        left = info['left']
        right = info['right']
        rang = range(left,right)
    except KeyError:
        columns = info['columns']
        rang = columns
    trace = []
    mash = info['mash']
    diff_pd = pd.DataFrame()
    month = time_days.shape[0]
    print(time_days.shape)
    for NUMBER in rang:
        y = []
        for j in range(1, 27):
            if j < 10:
                value_j = '0' + str(j)
            else:
                value_j = str(j)
            for i in range(1, 32):
                if i < 10:
                    value = '0' + str(i)
                else:
                    value = str(i)
                if i + 1 < 10:
                    value_1 = '0' + str(i + 1)
                else:
                    value_1 = str(i + 1)
                try:
                    y.append(time_days[NUMBER]['2020-' + value_j + '-' + value_1] - time_days[NUMBER][
                        '2020-' + value_j + '-' + value])
                    print('2020-' + value_j + '-' + value)
                except KeyError:
                    break
        diff_pd[NUMBER] = y
    trace = []
    for i in rang:
        trace2 = go.Scatter(
            x=diff_pd.index,
            y=diff_pd[i],
            name=str(i)
        )
        trace.append(trace2)
    data_plot = trace
    fit = go.Figure(data=data_plot)
    fit.update_yaxes(range=[mash[0], mash[1]])
    fit.show()


def plot_data(time_days, **info):
    """ График табличной фукции time_days. left и right - просматриваемые пункты (от и до) - тип int
    :param time_days: датафрейм , график которого нужно показать
    :param info: либо целочисленные left и right границы, или стоблцы, columns , которые стоит рассматривать как y
    """
    try:
        left = info['left']
        right = info['right']
        rang = range(left,right)
    except KeyError:
        columns = info['columns']
        rang = columns
    trace = []
    for i in rang:
        trace2 = go.Scatter(
            x=time_days[i].index,
            y=time_days[i],
            name=str(i)
        )
        trace.append(trace2)
    mash = info['mash']
    data_plot = trace
    fit = go.Figure(data=data_plot)
    fit.update_yaxes(range=[mash[0], mash[1]])
    fit.show()


def days(data_fr: 'Pandas data type', *column, DAT='Дата'):
    """ перевод минутного датафрейма в дневной (по дням) """
    if DAT == 'index':
        days = sorted(pd.to_datetime(list(map(lambda x: x.replace('.', '-') + '-20', {i[:5] for i in data_fr.index})),
                                     format='%d-%m-%y'))
        month = data_fr.shape[0]
    else:
        month = data_fr[DAT].__len__()
        days = [list(data_fr['Дата'])[i] for i in range(0,month,48)]
    mass_data = pd.DataFrame()
    for j in column:
        value_of_days = []
        for i in range(0, int(month / 48)):
            value_of_days.append(data_fr[j].iloc[list(range(48 * i, 48 * (i + 1), 1))].sum())
        mass_data[j] = value_of_days
    mass_data.index = days
    return mass_data


def read_file(path, **info):
    """:param info - info[left] - левая граница чтения файлов в каталоге (right - аналогично)
       :param path - путь к каталогу ексель файлов """
    left = info['left']
    right = info['right']
    Tr = info.get('Tr')
    engine = info.get('engine')
    skipr = info.get('skipr')
    if engine is None:
        engine = 'openpyxl'
    if skipr is None:
        skipr = 0
        print(os.listdir(path=path)[left:right])
        print(len(os.listdir(path=path)[left:right]))
    if Tr is None:
        new_list = reduce(pd.merge,
                          [pd.read_excel(path + r"\\" + i, engine=engine, skiprows=range(0, skipr)) for i in
                           os.listdir(path=path)[left:right]])
    else:
        new_list = pd.concat([pd.read_excel(path + r"\\" + i, engine=engine, skiprows=range(0, skipr)) for i in
                           os.listdir(path=path)[left:right]])
        new_list.sort_values('Дата',inplace=True)
    return new_list

def day_to_month(data):
    """ функция перевода датафрейма дней в датафрейм месяцев"""
    data_set_month = ()
    if len(data) == 366:
        data_set_month = (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    elif len(data) == 365:
        data_set_month = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    sum = 0
    new_data_set = pd.DataFrame()
    new_mass = []
    for i in data_set_month:
        value = pd.DataFrame(data.iloc[[i for i in range(sum, i + sum)]].sum(axis=0)).T
        new_mass.append(sum)
        sum += i
        if new_data_set.empty:
            new_data_set = value
        else:
            new_data_set = new_data_set.append(value)
    new_data_set.index = [data.index[i] for i in new_mass]
    return new_data_set