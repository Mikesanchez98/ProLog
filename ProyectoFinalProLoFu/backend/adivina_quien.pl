:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_write)).

% Cargar base de conocimiento
:- [personajes].  % Carga personajes.pl

% Configurar rutas HTTP
:- http_handler(root(.), serve_home, []).
:- http_handler('/guess', handle_guess, [method(post)]).
:- http_handler('/reset', handle_reset, []).
:- http_handler('/js/game.js', serve_js, []).
:- http_handler('/css/estilo.css', serve_css, []).

% Servir página principal
serve_home(_Request) :-
    reply_html_page(
        title('Adivina Quién Prolog'),
        [ meta([name(viewport), content('width=device-width, initial-scale=1.0')]),
          link([rel(stylesheet), href('/css/estilo.css')]),
          div([id('game-container')],
              [ h1('Adivina Quién'),
                div([id('character-image')], ''),
                div([id('question-area')], 'Piensa en un personaje...'),
                div([id('controls')],
                    [ button([id('yes-btn'), class('btn')], 'Sí'),
                      button([id('no-btn'), class('btn')], 'No'),
                      button([id('reset-btn'), class('btn')], 'Reiniciar')
                    ])
              ]),
          script([src('/js/game.js')], '')
        ]).

% Servir archivos estáticos - VERSIÓN CORREGIDA
serve_js(_Request) :-
    prolog_load_context(directory, Dir),
    atomic_list_concat([Dir, '/../frontend/js/game.js'], Path),
    http_reply_file(Path, [], []).

serve_css(_Request) :-
    prolog_load_context(directory, Dir),
    atomic_list_concat([Dir, '/../frontend/css/estilo.css'], Path),
    http_reply_file(Path, [], []).

% Resto del código permanece igual...

% Lógica del juego
remaining_characters(Chars) :-
    findall(P, (
        personaje(P, Attrs),
        forall(user_response(A, yes), member(A, Attrs)),
        forall(user_response(A, no), \+ member(A, Attrs))
    ), Chars).

next_question(Question) :-
    findall(A, (atributo(A), \+ user_response(A, _)), Available),
    (   Available = [] -> Question = none
    ;   random_member(Question, Available)
    ).

% Iniciar servidor
start_server :-
    http_server(http_dispatch, [port(8080)]).

:- initialization(start_server).