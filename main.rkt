#lang racket/gui

(require racket/draw)

; ========================================================================== Menu
; Cria um frame e escolhe o que vai estar escrito em cima
(define frameMenu (new frame%
                       [label "Menu"]
                       [width 960]
                       [height 540]))

; Cria a área desenhavel do Menu
(define menuCanva (new canvas%
                       [parent frameMenu]
                       [paint-callback
                        (lambda (canvas dc_aux)
                          (set! dc dc_aux)
                          (define background-jogo (make-object bitmap% "img/bgmenu.png"))
                          (send dc set-scale 1 1)
                          (send dc draw-bitmap background-jogo -25 0))]))

; Opção limita e ja definida de quantos ingredientes vai ter o hamburger que voce vai montar
(define choice (new choice%
                    (label "Quantidade de Ingredientes: ")
                    (parent frameMenu)
                    (choices (list "3" "4" "5" "6"))))

; Instancia o botão de começar do menu
; Ao ser clicado fechará a janela atual e abrirá uma nova janela
; Essa nova janela vai pegar o número da quantidade de ingredientes e vai começar um jogo
(define botaoComecar (new button%
                          [parent frameMenu]
                          [label "Começar"]
                          [callback (lambda (button event)
                                      (let ([selected-choice (send choice get-string-selection)])
                                        (set! torreCozinha (gerarTorre (string->number selected-choice) 0)) ;; gera torre da cozinha (hamburguer inicial)
                                        (send frameMenu show #f)
                                        (send frameJogo show #t)
                                        ))]))

; Instancia o botão de solucionar o problema de acordo com o número de ingredientes
; Ao ser clicado ele lerá o arquivo de solução referente a quantidade de ingredientes do hambuger
(define botaoSolucionar (new button%
                             [parent frameMenu]
                             [label "Solucionar"]
                             [callback (lambda (button event)
                                         (let ([selected-choice (send choice get-string-selection)])
                                           (define nome-arquivo (string-append "src/solucao" selected-choice ".txt"))
                                           ;ate aqui ok

                                           (define-values (esq dir) (le-arquivo nome-arquivo)) ;; seta valores (list) de origem  e destino com base no arquivo txt
                                           (set! direita_destino dir)
                                           (set! esquerda_origem esq)
                                           ;problema acima

                                           (set! torreCozinha (gerarTorre (string->number selected-choice) 0)) ;; gera torre da cozinha (hamburguer inicial)

                                           (send frameMenu show #f)
                                           (send frameAutomatico show #t)
                                           
                                           ;; seta o timer, que vai fazer as atualizações necessarias de redesenhar o canva  e etc
                                           (set! timer (new timer%
                                                            [interval 1000]
                                                            [notify-callback
                                                             (lambda ()
                                                               (define origem_element (last esquerda_origem)) ;; pega a origem da peça da etapa atual 
                                                               (define destino_element (last direita_destino)) ;; pega o destino da peça da etapa atual 
                                                               (set! esquerda_origem (reverse (cdr (reverse esquerda_origem)))) ;; remove o ultimo elemento da lista
                                                               (set! direita_destino (reverse (cdr (reverse direita_destino)))) ;; remove o ultimo elemento da lista
                                                               
                                                               ;;Se foi feita alguma movimentacao ele atualiza a tela, caso nao, ele atualiza e da gameOver
                                                               (if (refreshMovimentacoes origem_element destino_element) (atualizarTela dc) (gameOverAutomatico dc)))]))


                                           )
                                         )]))

; ========================================================================== Jogo

;; variavel referente a altura que vai ser acrescentada para colocar uma peça em cima da outra
(define constanteContador 15)

;; função que desenha as pecas de uma determinada torre
(define (desenharPecas dc lista_torre struct_torre contador)
  (send dc set-scale 2 2)
  (let ([peca (get-at lista_torre contador)])
    (cond
      [(peca? peca)
       (cond
         [(peca? (get-at lista_torre (add1 contador))) (desenharPecas dc lista_torre struct_torre (add1 contador))]
         [else #t])
       (send dc draw-bitmap (ingrediente-imagem (peca-ingrediente peca))  (torre-x struct_torre) (- (torre-y struct_torre) (* constanteContador contador)))]

      [else #t]
      )
    )
  )

;; funcao que realiza as transferencias de uma peça do topo de uma torre para o topo de outra torre
(define (refreshMovimentacoes origem destino)
  ; a cada 1s, while esquerda.at(pos)!=empty
  ;      transferir peça
  ;      desenharTorres
  (cond
    [(string=?  origem "Cozinha")
     (if (string=? destino "Garçom") (transferirCozinhaGarcom)
         (transferirCozinhaCliente))
     ]
    [(string=? origem "Garçom")
     (if (string=? destino "Cozinha") (transferirGarcomCozinha)
         (transferirGarcomCliente)
         )
     ]
    [(string=? origem "Cliente")
     (if (string=? destino "Cozinha") (transferirClienteCozinha)
         (transferirClienteGarcom))
     ]
    [else #f]
    )
  )

;; função que desenha todas as torres
(define (desenharTorres dc cozinha_lista  garcom_lista  cliente_lista cozinha_struct garcom_struct cliente_struct)
  (send dc set-scale 2 2)
  (desenharPecas dc cozinha_lista cozinha_struct 0)
  (desenharPecas dc garcom_lista garcom_struct 0)
  (desenharPecas dc  cliente_lista  cliente_struct 0)
  )

(define (desenhaPecaTopo dc destino_struct destino_lista peca)
  (send dc draw-bitmap (ingrediente-imagem (peca-ingrediente peca))  (torre-x destino_struct) (- (torre-y destino_struct) (* constanteContador (length destino_lista))))
  )

(define (transferirClienteGarcom)

  (if (empty? torreGarcom)
      (set! torreGarcom (cons (last torreCliente) empty))
      (set! torreGarcom (append torreGarcom (list (last torreCliente)))))
  (set! torreCliente (take torreCliente (- (length torreCliente) 1)))

  )


(define (transferirClienteCozinha)
  (if (empty? torreCozinha)
      (set! torreCozinha (cons (last torreCliente) empty))
      (set! torreCozinha (append torreCozinha (list (last torreCliente)))))

  (set! torreCliente (take torreCliente (- (length torreCliente) 1)))
  )

(define (transferirCozinhaGarcom)
  (if (empty? torreGarcom)
      (set! torreGarcom (cons (last torreCozinha) empty))
      (set! torreGarcom (append torreGarcom (list (last torreCozinha)))))

  (set! torreCozinha (take torreCozinha (- (length torreCozinha) 1)))
  )

(define (transferirCozinhaCliente)
  (if (empty? torreCliente)
      (set! torreCliente (cons (last torreCozinha) empty))
      (set! torreCliente (append torreCliente (list (last torreCozinha)))))

  (set! torreCozinha (take torreCozinha (- (length torreCozinha) 1)))

  )

(define (transferirGarcomCliente)
  (if (empty? torreCliente)
      (set! torreCliente (cons (last torreGarcom) empty))
      (set! torreCliente (append torreCliente (list (last torreGarcom)))))

  (set! torreGarcom (take torreGarcom (- (length torreGarcom) 1)))
  )

(define (transferirGarcomCozinha)
  (if (empty? torreCozinha)
      (set! torreCozinha (cons (last torreGarcom) empty))
      (set! torreCozinha (append torreCozinha (list (last torreGarcom)))))

  (set! torreGarcom (take torreGarcom (- (length torreGarcom) 1)))
  )
; ========================================================================== Estruturas de dados
(define-struct peca (valor ingrediente))
(define-struct ingrediente (nome imagem))
(define-struct torre (nome x y))

(define torreCozinha empty)
(define torreGarcom empty)
(define torreCliente empty)

(define cozinha (make-torre "Cozinha" 63 165))
(define garcom (make-torre "Garçom" 210 165))
(define cliente (make-torre "Cliente" 356 165))

(define esquerda_origem empty)

(define direita_destino empty)


; ========================================================================== Ingredientes do Hamburger
(define peca_valor2 (make-object bitmap% "img/carne.png"))
(define peca_valor3 (make-object bitmap% "img/queijo.png"))
(define peca_valor4 (make-object bitmap% "img/alface.png"))
(define peca_valor5 (make-object bitmap% "img/tomate.png"))


(define peca_pao_valor1 (make-object bitmap% "img/pao1.png"))
(define peca_pao_valor3 (make-object bitmap% "img/pao3.png"))
(define peca_pao_valor4 (make-object bitmap% "img/pao4.png"))
(define peca_pao_valor5 (make-object bitmap% "img/pao5.png"))
(define peca_pao_valor6 (make-object bitmap% "img/pao6.png"))

(define lista_paos (list
                    peca_pao_valor1 peca_pao_valor3 peca_pao_valor4 peca_pao_valor5 peca_pao_valor6
                    ))

(define paoBase (make-ingrediente "Pão" peca_pao_valor1))
(define queijo (make-ingrediente "Queijo" peca_valor3))
(define carne (make-ingrediente "Carne" peca_valor2))
(define tomate (make-ingrediente "Tomate" peca_valor5))
(define alface (make-ingrediente "Alface" peca_valor4))

(define lista-ingredientes (list paoBase carne queijo alface tomate))

; Função que gera torre de acordo com o valor fornecido pelo usuário
(define (gerarTorre n contador)
  (cond
    [(equal? n 1) (cons (make-peca 1 (make-ingrediente "PãoTopo" (get-at lista_paos (sub1 contador)))) empty)]
    [else (cons (make-peca n (get-at lista-ingredientes contador)) (gerarTorre (sub1 n) (add1 contador)))]
    )
  )

; ========================================================================== Util

;Essa função é chamada assim que é clicado o botão "Solucionar"
;A cada 1 segundo ele vai executar o que está dentro do [notify-callback]
(define dc empty)

(define timer empty)

(define (gameOverAutomatico dc)
  (desenharBackground dc)
  (desenharTorres dc torreCozinha torreGarcom torreCliente cozinha garcom cliente)
  (send timer stop)
  (printf "VOCE GANHOU")
  ;;ESCREVER ALGO COMO GAME OVER, VOCE GANHOU
  )

(define (atualizarTela dc)
  (desenharBackground dc)
  (desenharTorres dc torreCozinha torreGarcom torreCliente cozinha garcom cliente)
  )


(define (desenharBackground dc)
  (define background-jogo (make-object bitmap% "img/bgjogo.png"))
  (send dc set-scale 1 0.9)
  (send dc draw-bitmap background-jogo -25 0)
  )


(define (get-at lista pos)
  (cond
    [(empty? lista) empty]
    [(zero? pos) (first lista)]
    [else (get-at (rest lista) (sub1 pos))])
  )

(define (imprimir-peca peca)
  (if (peca? peca) (printf "Valor da peça: ~a / Nome da Peça: ~a\n " (peca-valor peca) (ingrediente-nome (peca-ingrediente peca))) (printf ""))
  )

(define (imprimir-pecas torre nomeTorre)
  (printf "Peças da torre ~a\n" nomeTorre)
  (for ([peca  torre])
    (imprimir-peca peca)
    ))

(define (le-arquivo nome-arquivo)
  (let ((esq '()) (dir '()))
    (for/list ([linha (in-list (file->lines nome-arquivo))])
      (let ((valores (string-split linha "|")))
        (set! esq (cons (car valores) esq))
        (set! dir (cons (cadr valores) dir))))
    (values esq dir)))


; ========================================================================== Interface
; Cria um frame para o jogo em si
(define frameJogo (new frame%
                       [label "Monte o hamburger na mesa do cliente!"]
                       [width 960]
                       [height 540]))

; Cria o frame para o computador jogar
(define frameAutomatico (new frame%
                             [label "Monte o hamburger na mesa do cliente!"]
                             [width 960]
                             [height 540]))

; Cria a área desenhavel do Jogo
(define jogoCanva (new canvas%
                       [parent frameJogo]
                       [paint-callback
                        (lambda (canvas dc_aux)
                          (set! dc dc_aux)
                          (send dc set-scale 2 2)
                          (send dc draw-text "Cozinha" 65 230)
                          (send dc draw-text "Garçom" 215 230)
                          (send dc draw-text "Cliente" 365 230)
                          (desenharBackground dc)
                          (desenharPecas  dc torreCozinha cozinha 0)

                          )]))

; Cria o frame pro computador jogar
(define jogoAutomatico (new canvas%
                            [parent frameAutomatico]
                            [paint-callback
                             (lambda (canvas dc_aux)
                               (set! dc dc_aux)
                               (send dc set-scale 2 2)
                               (send dc draw-text "Cozinha" 65 230)
                               (send dc draw-text "Garçom" 215 230)
                               (send dc draw-text "Cliente" 365 230)
                               (desenharBackground dc)
                               (desenharTorres dc torreCozinha torreGarcom torreCliente cozinha garcom cliente)
                               (send timer start 1000)
                               )]))

; Mostra a janela inicial do menu
(send frameMenu show #t)

