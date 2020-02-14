% Afonso Ribeiro, 89400



%------------------------------------------------------------------------
%				propaga/3				|
%------------------------------------------------------------------------


% A lista auxiliar instanciada vai guardar todos os elementos do termometro
% percorridos ate ao momento
propaga(Puz,Pos,Posicoes):-propaga(Puz,Pos,Posicoes,[]).


% Quando chega ao final de um termometro, avanca para o proximo, reiniciando
% tambem a lista auxiliar
propaga([[[]|C]|D],Pos,Posicoes,_):-propaga([C|D],Pos,Posicoes,[]).


% Percorre-se o termometro e procura-se a posicao indicada, se esta for encontrada
% ordena-se a lista auxiliar e para-se a recursao
propaga([[[Elem|Resto]|C]|D],Pos,Posicoes,Term):-
    Elem \= Pos ->
	propaga([[Resto|C]|D],Pos,Posicoes,[Elem|Term]);
    sort([Elem|Term],Posicoes).








%------------------------------------------------------------------------
%                     nao_altera_linhas_anteriores/3			|
%------------------------------------------------------------------------


% Instancia-se uma lista auxiliar que vai guardar os elementos de linhas
% anteriores
nao_altera_linhas_anteriores(Posicoes, L, Ja_Preenchidas):-
    nao_altera_linhas_anteriores(Posicoes, L, Ja_Preenchidas,[]).


% O predicado acaba com sucesso se a lista auxiliar for toda percorrida
nao_altera_linhas_anteriores([],_,_,[]).


% Verifica se todos os elementos da lista auxiliar pertencem 'a lista
% Ja_Preenchidas
nao_altera_linhas_anteriores([],L,Ja_Preenchidas,[A|Resto]):-
    member(A,Ja_Preenchidas),
    nao_altera_linhas_anteriores([],L,Ja_Preenchidas,Resto).


% Vai filtrando apenas os elementos de linhas anteriores e coloca-os na
% lista auxiliar
nao_altera_linhas_anteriores([(A,B)|Resto],L,Ja_Preenchidas,Certas):-
    A < L ->
    nao_altera_linhas_anteriores(Resto,L,Ja_Preenchidas,[(A,B)|Certas]);
    nao_altera_linhas_anteriores(Resto,L,Ja_Preenchidas,Certas).









%------------------------------------------------------------------------
%			verifica_parcial/4				|
%------------------------------------------------------------------------


% ----------------- PREDICADO PRINCIPAL -----------------


verifica_parcial(Puz, Ja_Preenchidas, Dim, Poss):-
    remove_duplicados(Ja_Preenchidas,Poss,Nova_Poss),
    verifica_parcial(Puz, Ja_Preenchidas, Dim, Nova_Poss, 0).


verifica_parcial(_,_,Dim,_,Dim).


% Testa a possibilidade atual (Poss) para cada coluna com o predicado
% verifica_aux/5. N tem o valor da coluna atual.
verifica_parcial([A,B,[Col|Resto]], Ja_Preenchidas, Dim, Poss, N):-
    N_Atual is N + 1,
    Cont is 0,
    verifica_aux(Ja_Preenchidas,Poss,N_Atual,Col,Cont),
    !,
    verifica_parcial([A,B,Resto], Ja_Preenchidas, Dim, Poss, N_Atual).







% ----------- PREDICADOS AUXILIARES -------------


% remove_duplicados/4 : remove da lista Poss os elementos que ja existem
% na lista Ja_Preenchidas, para que nao sejam contabilizados 2 vezes pelo
% predicado verifica_aux/5

remove_duplicados([],Poss,Poss_final):-
    Poss_final = Poss.

remove_duplicados([C|Resto],Poss,Poss_final):-
    delete(Poss,C,Nova_Poss),
    remove_duplicados(Resto,Nova_Poss,Poss_final).





% verifica_aux/5 verifica se a possibilidade atual (Poss) nao viola o total
% da coluna atual (Col), tendo em conta as escolhas anteriores (Ja_Preenchidas)

% Se tanto a lista Ja_Preenchidas como a lista Poss forem percorridas,
% temos uma possibilidade valida para a Coluna atual
verifica_aux([],[],_,_,_).


% Quando se chega ao final da lista Ja_Preenchidas, trocam-se as 2 listas,
%  para que se teste para a lista Poss
verifica_aux([],Lst1,N_Atual,Col,Cont):-verifica_aux(Lst1,[],N_Atual,Col,Cont).


% Se o primeiro elemento da lista tiver a Coluna = Col, aumenta-se o contador
% e continua-se a recursao, caso contrario, continua-se a recursao sem aumentar
% o contador. Se a qualquer altura o Contador for maior que o Total da coluna
% atual, o predicado falha, e a possibilidade (Poss) nao e' valida.
verifica_aux([(_,B)|Resto],Poss, N_Atual, Col, Cont):-
    B == N_Atual,
    !,
    Cont1 is Cont + 1,
    Cont1 =< Col,
    verifica_aux(Resto,Poss,N_Atual,Col,Cont1).

verifica_aux([(_,B)|Resto],Poss, N_Atual, Col, Cont):-
    B \= N_Atual,
    verifica_aux(Resto,Poss,N_Atual,Col,Cont).








% -----------------------------------------------------------
%                  possibilidades_linha/5                   |
% -----------------------------------------------------------



% ----------------- PREDICADO PRINCIPAL -----------------


% possibilidades_linha/5 :
%  - Encontram-se todas as combinacoes para a linha atual,
%  com tamanho = Total : Predicado combinacoes/3;
%  - Propagam-se todos os elementos e guardam-se as listas
%  propagadas: Predicado propaga_todos/3;
%  - Filtram-se as varias listas resultante consoante as regras
%  do enunciado: Predicado filtra_todos/8
possibilidades_linha(Puz, Posicoes_linha, Total,
                     Ja_Preenchidas,Possibilidades_L):-

    findall(Poss,combinacoes(Posicoes_linha,Poss,Total),Todos_combinados),

    propaga_todos(Puz,Todos_combinados,Todos_propagados),

    proper_length(Posicoes_linha, Dim),
    filtra_todos(Puz,Posicoes_linha,Ja_Preenchidas,Todos_propagados,
                 Dim, Total,[],Possibilidades_L).





% ----------- PREDICADOS AUXILIARES -------------

% combinacoes/3 : Determina todas as combinacoes possiveis para a
% linha dada e com tamanho = Total

combinacoes(Posicoes_linha,Poss,Total):-
    combinacoes(Posicoes_linha,Poss),
    length(Poss,Total). % O tamanho da lista tem q ser igual ao Total

combinacoes([],[]).


% Cria todas as combinacoes possiveis, atraves de multiplos retrocessos
% com o findall
combinacoes([C|Resto1],[C|Resto2]):-
    combinacoes(Resto1,Resto2).

combinacoes([_|Resto1],Poss):-
    combinacoes(Resto1,Poss).







% propaga_todos/3 : Propaga cada elemento de cada lista resultante do
% predicado combinacoes/3

propaga_todos(Puz,Todos_combinados,A):-
    propaga_todos(Puz,Todos_combinados,A,[]).


propaga_todos(_,[[]|[]],[List_B],List_aux):-
    append(List_aux,List_A),
    sort(List_A,List_B).

propaga_todos(Puz,[[]|Resto],[List_B|Todos_propagados],List_aux):-
    append(List_aux,List_A),
    sort(List_A,List_B),
    propaga_todos(Puz,Resto,Todos_propagados,[]).

propaga_todos(Puz,[[C|Resto1]|Resto2],Todos_propagados,List_aux):-
    propaga(Puz,C,List_A),
    propaga_todos(Puz,[Resto1|Resto2],Todos_propagados,[List_A|List_aux]).







% filtra_todos/7 : filtra as listas propagadas consoante as
% varias regras do enunciado.
% Tem 3 predicados auxiliares: teste_filtra/7, pertence/2 e filtra_tamanho/4

filtra_todos(_,_,_,[],_,_,Todos_aux,Possibilidades_L):-
    sort(Todos_aux,Possibilidades_L).



filtra_todos(Puz,Posicoes_linha,Ja_Preenchidas,[C|Resto],
                 Dim, Total,Todos_aux,Possibilidades_L):-

    intersection(Posicoes_linha,Ja_Preenchidas,L_Ja_Preenchidas),
    member((L,_),Posicoes_linha),		% L guarda o numero da linha

    (teste_filtra(L_Ja_Preenchidas,C,L,Puz,Ja_Preenchidas,Dim,Total) ->

    filtra_todos(Puz,Posicoes_linha,Ja_Preenchidas,Resto,
                 Dim, Total,[C|Todos_aux],Possibilidades_L);

    filtra_todos(Puz,Posicoes_linha,Ja_Preenchidas,Resto,
                 Dim, Total,Todos_aux,Possibilidades_L)).





% teste_filtra/7 testa se uma dada possibilidade cumpre todos os requesitos
% do enunciado para ser uma linha valida, sendo esses requesitos :
%
%  - Ter o tamanho certo: testado pelo predicado filtra_tamanho/4;
%  - Os elementos de Ja_Preenchidas da linha em questao pertencem obrigatoriamente
%  a possibilidade: testado pelo predicado pertence/2;
%  - A linha propagada nao pode ter elementos de outras linhas que nao estejam na
%  lista Ja_Preenchidas: testado pelo predicado nao_altera_linhas_anteriores/3;
%  - Os elementos da linha tem que respeitar os totais das colunas: testado
%  pelo predicado verifica_parcial/4.



teste_filtra(L_Ja_Preenchidas,Lista,L,Puz,Ja_Preenchidas,Dim,Total):-
    filtra_tamanho(Lista,L,Total,0),
    pertence(L_Ja_Preenchidas,Lista),
    nao_altera_linhas_anteriores(Lista,L,Ja_Preenchidas),
    verifica_parcial(Puz,Ja_Preenchidas,Dim,Lista).




filtra_tamanho([],_,Total,Counter):-
    !,
    Total == Counter.

filtra_tamanho([(A,_)|Resto],L,Total,Counter):-
    A == L,
    !,
    Counter1 is Counter + 1,
    Counter =< Total,   % Falha se o Contador for maior que o Total
    filtra_tamanho(Resto,L,Total,Counter1).

filtra_tamanho([(A,_)|Resto],L,Total,Counter):-
    A \= L,
    filtra_tamanho(Resto,L,Total,Counter).




pertence([],_).

pertence([A|Resto],Lista):-
   member(A,Lista),
   pertence(Resto,Lista).












%------------------------------------------------------------------------
%				resolve/2				|
%------------------------------------------------------------------------


% resolve/2 : Corre o predicado possibilidades_linha/5 para cada linha
% do Puzzle, guardando as linhas que vao sendo preenchidas na lista
% Ja_Preenchidas.
% Se o possibilidades_linha/5 falhar, recua-se ate' ao ultimo predicado
% member/2 com mais do que uma possibilidade e corre-se outra vez a
% partir dessa possibilidade.
resolve([A,L,C],Solucao):-
    proper_length(L,Dim),
    resolve([A,L,C],Solucao,[],Dim,0).
    % Instancia-se a lista Ja_Preenchidas, a Dimensao do Puzzle e um contador
    % de linhas

resolve(_,Solucao,Ja_Preenchidas,Dim,Dim):-
    sort(Ja_Preenchidas,Solucao).


resolve([A,L,C],Solucao,Ja_Preenchidas,Dim,Counter):-
    Counter_novo is Counter + 1,
    linha(Counter_novo,Dim,0,Posicoes_linha),
    nth1(Counter_novo,L,Total),

    possibilidades_linha([A,L,C], Posicoes_linha, Total,
                         Ja_Preenchidas, Possibilidades_L),
    !,
    member(Poss,Possibilidades_L),

    append(Poss,Ja_Preenchidas,Ja_Preenchidas2),
    sort(Ja_Preenchidas2,Ja_Preenchidas_final),
    resolve([A,L,C],Solucao,Ja_Preenchidas_final,Dim,Counter_novo).



% linha/4 : Cria a lista Posicoes_linha
linha(_,Dim,Dim,[]).

linha(Linha,Dim,Coluna,[C|Resto]):-
    Coluna_nova is Coluna + 1,
    C = (Linha,Coluna_nova),
    linha(Linha,Dim,Coluna_nova,Resto).




























