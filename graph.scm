(native dagreD3.Digraph)
(native dagreD3.Renderer)

(define graph-data '((0 "Is this the right room for an argument?" () ())
                     (1 "I've told you once." (0) ())
                     (2 "No you haven't!" (1) ())
                     (3 "Yes I have." (2) ())
                     (4 "When?" (3) ())
                     (5 "Just now. " (4) ())
                     (6 "No you didn't! " (5) ())
                     (7 "Yes I did! " (6) ())
                     (8 "You didn't! " (7) ())
                     (9 "I did! " (8) ())
                     (10 "You didn't! " (9) ())
                     (11 "I'm telling you, I did! " (10) ())
                     (12 "You didn't! " (11) ())

                     (53 "I came here for a good argument!" () ())
                     (54 "AH, no you didn't, you came here for an argument!" (53) ())

                     (55 "An argument isn't just contradiction." () ())
                     (56 "Well!  it CAN be!" (55) ())
                     (57 "No it can't!" (56) ())
                     (58 "An argument is a connected series of statement intended to establish a proposition." (57) (55))
                     (59 "No it isn't!" (58) ())
                     (60 "Yes it is!  'tisn't just contradiction." (59) (55))

                     (61 "Look, if I *argue* with you, I must take up a contrary position!" () ())
                     (62 "Yes but it isn't just saying \"no it isn't\"."  (61) ())
                     (63 "Yes it is! " (62) ())
                     (64 "No it isn't!"  (63) ())
                     (65 "Yes it is! " (64) ())
                     (66 "No it isn't!"  (65) ())
                     (67 "Yes it is! " (66) ())

                     (68 "No it ISN'T!  Argument is an intellectual process.  Contradiction is just the automatic gainsaying of anything the other person says. " (66) (58 60))
                     (69 "It is NOT! " (68) ())
                     (70 "It is! " (69) ())
                     (71 "Not at all! " (70) ())
                     (72 "It is! " (71) ())))

(define (get-argument id)
  (assq id graph-data))

(define rendered-nodes '())

(define (add-node graph id label)
  (%inline ".addNode" graph id (% "label" label)))

(define (add-edge graph id1 id2 label)
  (%inline ".addEdge" graph '() id1 id2 (% "label" label)))

(define (add-node/if-not-exists graph id content)
  (unless (member id rendered-nodes)
    (add-node graph id content)
    (set! rendered-nodes (cons id rendered-nodes))))

(define (add-edge* graph node1-id node1-content node2 label)
  (match node2
         ((node2-id node2-content _ _)
          (add-node/if-not-exists graph node1-id node1-content)
          (add-node/if-not-exists graph node2-id node2-content)
          (add-edge graph node1-id node2-id label))))

(define (init)
  (let ((g              (new dagreD3.Digraph))
        (render         (let* ((renderer (new dagreD3.Renderer))
                               (svg      (%inline "d3.select" "svg g"))
                               (layout   (%inline "dagreD3.layout"))
                               (renderer (%inline ".layout" renderer layout)))
                          (lambda (g) 
                            (%inline ".run" renderer g svg))))
        (add-relations  (lambda (g id content arguments label)
                          (for-each
                            (lambda (argument-id)
                              (add-edge* g id content (get-argument argument-id) label))
                            arguments))))
    (for-each
      (match-lambda 
        ((id content against in-favor)
         (add-node/if-not-exists g id content)
         (add-relations g id content against "against")
         (add-relations g id content in-favor "in-favor")))
      graph-data)
    (render g)))

(%host-set! init (callback init))
