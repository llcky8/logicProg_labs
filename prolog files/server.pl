:- encoding(utf8).

:- use_module(library(http/http_server)).        % для запуска HTTP-сервера
:- use_module(library(http/html_write)).         % для генерации HTML-страниц
:- use_module(library(http/http_dispatch)).      % для маршрутов (URL -> обработчик)
:- use_module(library(http/http_parameters)).    % для чтения параметров из формы
:- dynamic car/5.                                % база данных машин, изменяемая

:- http_handler(root(.), home_page, []).
:- http_handler(root(init_db), init_db, [method(post)]).
:- http_handler(root(add), add_page, []).
:- http_handler(root(add_car), add_car, [method(post)]).
:- http_handler(root(delete_car), delete_car, [method(post)]).
:- http_handler(root(search_color), search_color_page, []).
:- http_handler(root(search_result), search_result, [method(get)]).
:- http_handler(root(min_price), min_price_page, []).


server :- http_server(http_dispatch, [port(8080)]).

init_db(Request) :-
    retractall(car(_,_,_,_,_)),
    assert(car('AutoWorld', 'Toyota', 'Corolla', 15000, 'красный')),
    assert(car('SpeedMotors', 'BMW', 'X5', 40000, 'черный')),
    assert(car('AutoWorld', 'Honda', 'Civic', 18000, 'голубой')),
    assert(car('MegaCars', 'Ford', 'Focus', 16000, 'красный')),
    assert(car('EuroAuto', 'Renault', 'Megane', 14000, 'белый')),
    assert(car('EuroAuto', 'Peugeot', '208', 13500, 'серый')),
    assert(car('SpeedMotors', 'Audi', 'A4', 30000, 'синий')),
    assert(car('NipponDrive', 'Nissan', 'Leaf', 25000, 'зеленый')),
    assert(car('NipponDrive', 'Mazda', 'CX-5', 28000, 'черный')),
    assert(car('AutoWorld', 'Chevrolet', 'Malibu', 17000, 'белый')),
    assert(car('MegaCars', 'Ford', 'Explorer', 32000, 'синий')),
    assert(car('SpeedMotors', 'Volkswagen', 'Golf', 20000, 'серебристый')),
    assert(car('EuroAuto', 'Citroen', 'C3', 13000, 'желтый')),
    assert(car('NipponDrive', 'Subaru', 'Forester', 27000, 'зеленый')),
    assert(car('MegaCars', 'Tesla', 'Model 3', 35000, 'белый')),
    http_redirect(moved, '/', Request).

home_page(_Request) :-
    findall(tr([td(Firm), td(Brand), td(Model), td(Price), td(Color)]),
            car(Firm, Brand, Model, Price, Color),
            Rows),
    reply_html_page(
        title('База данных автомобилей'),
        [
            h1('Продажа автомобилей'),
            table([border(1)],
                  [tr([th('Фирма'), th('Марка'), th('Модель'), th('Цена'), th('Цвет')])|Rows]),
            p([
                form([action('/add'), method(get)],
                     button([type=submit], 'Добавить автомобиль')),
                form([action('/init_db'), method(post), style('display:inline')],
                     button([type=submit], 'Сброс БД к исходной')),
                form([action('/search_color'), method(get), style('display:inline')],
                     button([type=submit], 'Поиск по цвету')),
                form([action('/min_price'), method(get), style('display:inline')],
                     button([type=submit], 'Минимальная цена автомобиля'))
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
        title('Add Car'),
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
    assertz(car(Firm, Brand, Model, Price, Color)),
    http_redirect(moved, '/', Request).

delete_car(Request) :-
    http_parameters(Request, [
        firm(Firm, []),
        model(Model, [])
    ]),
    retractall(car(Firm, _, Model, _, _)),
    http_redirect(moved, '/', Request).

search_color_page(_Request) :-
    reply_html_page(
        title('Поиск по цвету'),
        form([action('/search_result'), method(get)],
             [
                 p(['Ввести цвет: ', input([name=color]), input([type=submit, value='Поиск'])]),
                 p(a(href('/'), 'Назад'))
             ])).

search_result(Request) :-
    http_parameters(Request, [color(Color, [])]),
    findall(Firm,
            car(Firm, _, _, _, Color),
            Firms),
    list_to_set(Firms, UniqueFirms),
    findall(li(F), member(F, UniqueFirms), Items),
    reply_html_page(
        title('Firms by Color'),
        [
            h1(['Все фирмы, которые продают автомобили заданной расцветки: ', Color]),
            ul(Items),
            p(a(href('/'), 'Назад'))
        ]).

min_price_page(_Request) :-
    findall(Price-Brand-Model, car(_, Brand, Model, Price, _), Cars),
    sort(Cars, [MinPrice-Brand-MinModel|_]),
    reply_html_page(
        title('Автомобили с минимальной стоимость'),
        [
            h1('Список автомобилей'),
            p(['Марка: ', Brand]),
            p(['Модель: ', MinModel]),
            p(['Цена: $', MinPrice]),
            p(a(href('/'), 'Назад'))
        ]).
