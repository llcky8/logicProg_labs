:- encoding(utf8).
:- use_module(library(http/http_server)).     % для запуска HTTP-сервера
:- use_module(library(http/html_write)).      % для генерации HTML-страниц
:- use_module(library(http/http_dispatch)).   % для маршрутов (URL -> обработчик)
:- use_module(library(http/http_parameters)). % для чтения параметров из формы
:- use_module(library(odbc)).                 % для базы данных машин

:- http_handler(root(.), home_page, []).
:- http_handler(root(init_db), init_db, [method(post)]).
:- http_handler(root(add), add_page, []).
:- http_handler(root(add_car), add_car, [method(post)]).
:- http_handler(root(delete_car), delete_car, [method(post)]).
:- http_handler(root(search_color), search_color_page, []).
:- http_handler(root(search_result), search_result, [method(get)]).
:- http_handler(root(min_price), min_price_page, []).

server :- http_server(http_dispatch, [port(8080)]).

connect_db :-
    odbc_connect('car_db', _Connection,
                 [ user('root'),
                   password('admin'),
                   alias(cardb),
                   open(once)
                 ]).


init_db(Request) :-
    connect_db,
    odbc_query(cardb, 'DELETE FROM cars'),
    odbc_query(cardb, "INSERT INTO cars (firm, brand, model, price, color) VALUES
        ('AutoWorld', 'Toyota', 'Corolla', 15000, 'красный'),
        ('SpeedMotors', 'BMW', 'X5', 40000, 'черный'),
        ('AutoWorld', 'Honda', 'Civic', 18000, 'голубой'),
        ('MegaCars', 'Ford', 'Focus', 16000, 'красный'),
        ('EuroAuto', 'Renault', 'Megane', 14000, 'белый'),
        ('EuroAuto', 'Peugeot', '208', 13500, 'серый'),
        ('SpeedMotors', 'Audi', 'A4', 30000, 'синий'),
        ('NipponDrive', 'Nissan', 'Leaf', 25000, 'зеленый'),
        ('NipponDrive', 'Mazda', 'CX-5', 28000, 'черный'),
        ('AutoWorld', 'Chevrolet', 'Malibu', 17000, 'белый'),
        ('MegaCars', 'Ford', 'Explorer', 32000, 'синий'),
        ('SpeedMotors', 'Volkswagen', 'Golf', 20000, 'серебристый'),
        ('EuroAuto', 'Citroen', 'C3', 13000, 'желтый'),
        ('NipponDrive', 'Subaru', 'Forester', 27000, 'зеленый'),
        ('MegaCars', 'Tesla', 'Model 3', 35000, 'белый')"
    ),
    http_redirect(moved, '/', Request).

home_page(_Request) :-
    connect_db,
    findall(tr([td(Firm), td(Brand), td(Model), td(Price), td(Color)]),
            odbc_query(cardb,
                       'SELECT firm, brand, model, price, color FROM cars',
                       row(Firm, Brand, Model, Price, Color)),
            Rows),
    reply_html_page(
        title('База данных автомобилей'),
        [
            h1('Продажа автомобилей'),
            table([border(1)],
                  [tr([th('Фирма'), th('Марка'), th('Модель'), th('Цена'), th('Цвет')])|Rows]),
            p([
                form([action('/add'), method(get)], button([type=submit], 'Добавить автомобиль')),
                form([action('/init_db'), method(post), style('display:inline')],
                     button([type=submit], 'Сброс БД к исходной')),
                form([action('/search_color'), method(get), style('display:inline')],
                     button([type=submit], 'Поиск по цвету')),
                form([action('/min_price'), method(get), style('display:inline')],
                     button([type=submit], 'Минимальная цена авто'))
            ]),
            form([action('/delete_car'), method(post)],
                 table([
                     tr([th('Фирма'), td(input([name=firm]))]),
                     tr([th('Модель'), td(input([name=model]))]),
                     tr(td([colspan(2), align(right)],
                           input([type=submit, value='Удалить'])))
                 ]))
        ]).

add_page(_Request) :-
    reply_html_page(
        title('Добавить авто'),
        form([action('/add_car'), method(post)],
             table([
                 tr([th('Фирма'), td(input([name=firm]))]),
                 tr([th('Марка'), td(input([name=brand]))]),
                 tr([th('Модель'), td(input([name=model]))]),
                 tr([th('Цена'), td(input([name=price]))]),
                 tr([th('Цвет'), td(input([name=color]))]),
                 tr(td([colspan(2), align(right)],
                       input([type=submit, value='Добавить']))),
                 tr(td(a(href('/'), 'Назад')))
             ]))).

add_car(Request) :-
    http_parameters(Request, [
        firm(Firm, []),
        brand(Brand, []),
        model(Model, []),
        price(PriceAtom, []),
        color(Color, [])
    ]),
    atom_number(PriceAtom, Price),
    connect_db,
    format(atom(Query),
           "INSERT INTO cars (firm, brand, model, price, color) VALUES ('~w', '~w', '~w', ~w, '~w')",
           [Firm, Brand, Model, Price, Color]),
    odbc_query(cardb, Query),
    http_redirect(moved, '/', Request).

delete_car(Request) :-
    http_parameters(Request, [
        firm(Firm, []),
        model(Model, [])
    ]),
    connect_db,
    format(atom(Query),
           "DELETE FROM cars WHERE firm='~w' AND model='~w'", [Firm, Model]),
    odbc_query(cardb, Query),
    http_redirect(moved, '/', Request).

search_color_page(_Request) :-
    reply_html_page(
        title('Поиск по цвету'),
        form([action('/search_result'), method(get)],
             [
                 p(['Введите цвет: ', input([name=color]), input([type=submit, value='Поиск'])]),
                 p(a(href('/'), 'Назад'))
             ])).

search_result(Request) :-
    http_parameters(Request, [color(Color, [])]),
    connect_db,
    findall(li(Firm),
            odbc_query(cardb,
                       "SELECT DISTINCT firm FROM cars WHERE color = ?",
                       row(Firm),
                       [parameters([Color])]),
            Firms),
    reply_html_page(
        title('Результаты поиска'),
        [
            h1(['Фирмы, продающие автомобили цвета ', Color]),
            ul(Firms),
            p(a(href('/'), 'Назад'))
        ]).

min_price_page(_Request) :-
    connect_db,
    odbc_query(cardb,
               "SELECT brand, model, price FROM cars ORDER BY price ASC LIMIT 1",
               row(Brand, Model, Price)),
    reply_html_page(
        title('Минимальная цена'),
        [
            h1('Самый дешёвый автомобиль'),
            p(['Марка: ', Brand]),
            p(['Модель: ', Model]),
            p(['Цена: $', Price]),
            p(a(href('/'), 'Назад'))
        ]).