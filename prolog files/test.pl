:- use_module(library(odbc)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

% Запускаем сервер на порту 8080
:- http_server(http_dispatch, [port(8080)]).

% Регистрируем обработчик пути /
:- http_handler(root(.), students_table_handler, []).

% Подключение к базе
connect_db :-
    odbc_connect('my_sqlite_db', _, [alias(db), open(once), encoding(utf8)]).

% Основной обработчик запроса
students_table_handler(_Request) :-
    connect_db,
    findall(row(Id, Name, Age),
            odbc_query(db, 'SELECT id, name, age FROM student', row(Id, Name, Age)),
            Rows),
    reply_html_page(
        title('Список студентов'),
        \students_table(Rows)).

% HTML-шаблон таблицы
students_table(Rows) -->
    html([
        h1('Список студентов'),
        table([border(1), cellpadding(5)],
            [ tr([th('ID'), th('Имя'), th('Возраст')])
            | \table_rows(Rows)
            ])
    ]).

% Преобразование строк в tr/td
table_rows([]) --> [].
table_rows([row(Id, Name, Age)|T]) -->
    html(tr([td(Id), td(Name), td(Age)])),
    table_rows(T).
