:- encoding(utf8).
:- use_module(library(sgml)).      % для работы с XML
:- use_module(library(date)).      % для времени логов

:- dynamic car/5.

log_action(Action) :-                                   % логирование
    get_time(TimeStamp),
    stamp_date_time(TimeStamp, DateTime, 'UTC'),
    format_time(atom(TimeStr), '%Y-%m-%d %H:%M:%S', DateTime),
    open('actions.log', append, Stream),
    format(Stream, '~w - ~w~n', [TimeStr, Action]),
    close(Stream).

load_cars_from_xml(File) :-                             % загрузка из XML
    load_xml_file(File, [element(cars, _, RawCars)]),
    include(is_car_element, RawCars, Cars),
    retractall(car(_, _, _, _, _)),
    forall(member(element(car, _, [
        element(seller, _, [SellerText]),
        element(brand, _, [BrandText]),
        element(model, _, [ModelText]),
        element(price, _, [PriceText]),
        element(color, _, [ColorText])
    ]), Cars),
    (
        atom_number(PriceText, Price),
        assertz(car(SellerText, BrandText, ModelText, Price, ColorText))
    )),
    log_action('Загрузка данных из XML').

is_car_element(element(car, _, _)).

is_car_element(element(car, _, _)).
    
save_cars_to_xml(File) :-                              % сохранение в XML
    findall(
        element(car, [], [
            element(seller, [], [Seller]),
            element(brand, [], [Brand]),
            element(model, [], [Model]),
            element(price, [], [PriceStr]),
            element(color, [], [Color])
        ]),
        (
            car(Seller, Brand, Model, Price, Color),
            number_string(Price, PriceStr)
        ),
        Cars
    ),
    XML = [element(cars, [], Cars)],
    open(File, write, Stream, [encoding(utf8)]),
    xml_write(Stream, XML, [header(true)]),
    close(Stream),
    log_action('Сохранение данных в XML').


add_car(Seller, Brand, Model, Price, Color) :-              % добавление и удаление
    assertz(car(Seller, Brand, Model, Price, Color)),
    log_action('Добавлен новый автомобиль').

delete_car(Brand, Model) :-
    retract(car(_, Brand, Model, _, _)),
    log_action('Удалён автомобиль').

clear_db :-
    retractall(car(_, _, _, _, _)),
    log_action('Очищена база фактов').

query_by_brand(Brand) :-
    log_action('Поиск машин по бренду'),
    forall(car(Seller, Brand, Model, Price, Color),
           format('~w ~w ~w ~w ~w~n', [Seller, Brand, Model, Price, Color])).

init_db(File) :-
    load_cars_from_xml(File),
    log_action('Инициализация базы из XML').

check_xml(File) :-
    load_xml_file(File, Data),
    writeln('== RAW XML STRUCTURE =='),
    print(Data).
