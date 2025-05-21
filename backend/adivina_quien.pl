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

% Dynamic predicates for game state
:- dynamic user_response/2.

% Handle guess requests with proper error handling
handle_guess(Request) :-
    catch(
        handle_guess_internal(Request),
        Error,
        reply_json_dict(_{error: true, message: Error})
    ).

handle_guess_internal(Request) :-
    (   http_read_json_dict(Request, Data)
    ->  (   get_dict(attribute, Data, Attribute)
        ->  get_dict(answer, Data, Answer),
            assertz(user_response(Attribute, Answer))
        ;   true
        ),
        remaining_characters(Chars),
        length(Chars, NumChars),
        (   NumChars = 1
        ->  [Character] = Chars,
            character_description(Character, Description),
            reply_json_dict(_{
                gameOver: true,
                solution: Character,
                description: Description
            })
        ;   (   next_question(NextQ)
            ->  reply_json_dict(_{
                    gameOver: false,
                    nextQuestion: NextQ,
                    remainingCharacters: NumChars
                })
            ;   reply_json_dict(_{
                    error: true,
                    message: "No more questions available"
                })
            )
        )
    ;   reply_json_dict(_{
            error: true,
            message: "Invalid JSON request"
        })
    ).

% Handle reset requests with error handling
handle_reset(_Request) :-
    catch(
        (retractall(user_response(_, _)),
        reply_json_dict(_{status: "ok"})),
        Error,
        reply_json_dict(_{error: true, message: Error})
    ).

% Get character description
character_description(Character, Description) :-
    personaje(Character, Attrs),
    attrs_to_description(Attrs, Description).

% Convert attributes to readable description
attrs_to_description(Attrs, Description) :-
    findall(Desc, (
        member(Attr, Attrs),
        attr_to_text(Attr, Desc)
    ), Descriptions),
    atomic_list_concat(Descriptions, ', ', Description).

% Convert attribute to readable text
attr_to_text(hombre, "es un hombre").
attr_to_text(mujer, "es una mujer").
attr_to_text(pelo_negro, "tiene pelo negro").
attr_to_text(pelo_rubio, "tiene pelo rubio").
attr_to_text(pelo_rojo, "tiene pelo rojo").
attr_to_text(pelo_castaño, "tiene pelo castaño").
attr_to_text(pelo_morado, "tiene pelo morado").
attr_to_text(pelo_rosa, "tiene pelo rosa").
attr_to_text(pelo_verde, "tiene pelo verde").
attr_to_text(pelo_azul, "tiene pelo azul").
attr_to_text(pelo_blanco, "tiene pelo blanco").
attr_to_text(pelo_cano, "tiene pelo cano").
attr_to_text(pelo_gris, "tiene pelo gris").
attr_to_text(pelo_plateado, "tiene pelo plateado").
attr_to_text(ojos_cafe, "tiene ojos café").
attr_to_text(ojos_azules, "tiene ojos azules").
attr_to_text(ojos_verdes, "tiene ojos verdes").
attr_to_text(ojos_negros, "tiene ojos negros").
attr_to_text(ojos_grises, "tiene ojos grises").
attr_to_text(ojos_avellana, "tiene ojos avellana").
attr_to_text(usa_lentes, "usa lentes").
attr_to_text(no_lentes, "no usa lentes").
attr_to_text(alto, "es alto").
attr_to_text(bajo, "es bajo").
attr_to_text(mediano, "es de estatura mediana").
attr_to_text(alta, "es alta").
attr_to_text(baja, "es baja").
attr_to_text(mediana, "es de estatura mediana").
attr_to_text(Attr, Text) :- atom_concat(_, Attr, Text).

% Lógica del juego
remaining_characters(Chars) :-
    findall(P, (
        personaje(P, Attrs),
        forall(user_response(A, yes), member(A, Attrs)),
        forall(user_response(A, no), \+ member(A, Attrs))
    ), Chars).

next_question(Question) :-
    findall(A, (atributo(A), \+ user_response(A, _)), Available),
    (Available = [] -> Question = none
    ; random_member(Question, Available)).

% Iniciar servidor
start_server :-
    http_server(http_dispatch, [port(3000)]).

:- initialization(start_server).