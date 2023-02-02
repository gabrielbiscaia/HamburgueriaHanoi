#lang racket/gui

; Make a frame by instantiating the frame% class
(define frame (new frame% [label "Hamburgueria de Hanói"]))
 
; Make a static text message in the frame
(define msg (new message% [parent frame]
                          [label "Hamburgueria de Hanói"]))
 
; Make a button in the frame
(new button% [parent frame]
             [label "Começar"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send msg set-label "Começou o jogo!"))])

; Derive a new canvas (a drawing window) class to handle events
(define my-canvas%
  (class canvas% ; The base class is canvas%
    ; Define overriding method to handle mouse events
    (define/override (on-event event)
      (send msg set-label "Evento de mouse ativo"))
    ; Define overriding method to handle keyboard events
    (define/override (on-char event)
      (send msg set-label "Evento de teclado ativo"))
    ; Call the superclass init, passing on all init args
    (super-new)))
 
; Make a canvas that handles events in the frame
(new my-canvas% [parent frame])
 
; Show the frame by calling its show method
(send frame show #t)