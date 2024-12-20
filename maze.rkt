;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname maze) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)

;; ==================
;; CONSTANTS

(define SQUARE-SIDE 40)
(define NEXT-POS-COLOR "yellow")
(define NEXT-PATH-COLOR "orange")
(define WORKLIST-COLOR "blue")
(define VISITED-COLOR "purple")
(define OBSTACLE-COLOR "black")
(define FREE-COLOR "white")

;; ===================
;; DATA DEFINITIONS

(@htdd Position)
(define-struct pos (x y))
;; Position is (make-pos Natural Natural).
;; interp. Position (x. y) in a given map.
(define POS0 (make-pos 0 0))
(define POS1 (make-pos 2 1))
(define POS2 (make-pos 3 3))
(define POS3 (make-pos 0 4))

#;
(define (fn-for-pos pos)
  (... (pos-x pos)
       (pos-y pos)))

(@htdd TileType)
;; TileType is one of:
;; - "obstacle"
;; - "free"
;; interp. An obstacle if "obstacle", an available tile if "free".
(define TILEOBS "obstacle")
(define TILEFREE "free")

#;
(define (fn-for-t t)
  (cond [(string=? t "obstacle") (...)]
        [else (...)]))

(@htdd Map)
;; Map is (listof (listof TileType))
;; interp. A nxn matrix of tiles that represents potential obstacles.
;; CONSTRAINT: The primary map list is the same size as all sublists.
(define MAPE empty) ; note that (list empty) would be invalid.
(define MAP0 (list (list TILEOBS)))
(define MAP1 (list (list TILEFREE)))
(define MAP2 (list (list TILEFREE TILEFREE)
                   (list TILEOBS TILEFREE)))
(define MAP3 (list (list TILEFREE TILEFREE)
                   (list TILEFREE TILEFREE)))
(define MAP4 (list (list TILEFREE TILEFREE TILEFREE TILEFREE TILEFREE)
                   (list TILEFREE TILEOBS TILEFREE TILEOBS TILEFREE)
                   (list TILEFREE TILEFREE TILEFREE TILEOBS TILEFREE)
                   (list TILEFREE TILEOBS TILEFREE TILEOBS TILEFREE)
                   (list TILEFREE TILEOBS TILEFREE TILEOBS TILEFREE)))
(define MAP5 (list (list TILEFREE TILEFREE TILEFREE TILEFREE TILEFREE TILEFREE
                         TILEOBS TILEFREE)
                   (list TILEFREE TILEOBS TILEFREE TILEOBS TILEFREE TILEFREE
                         TILEOBS TILEFREE)
                   (list TILEFREE TILEFREE TILEFREE TILEOBS TILEFREE TILEFREE
                         TILEOBS TILEFREE)
                   (list TILEFREE TILEOBS TILEFREE TILEOBS TILEFREE TILEFREE
                         TILEOBS TILEFREE)
                   (list TILEFREE TILEOBS TILEFREE TILEOBS TILEFREE TILEFREE
                         TILEOBS TILEFREE)
                   (list TILEFREE TILEOBS TILEFREE TILEOBS TILEFREE TILEFREE
                         TILEOBS TILEFREE)
                   (list TILEFREE TILEOBS TILEFREE TILEOBS TILEFREE TILEFREE
                         TILEOBS TILEFREE)
                   (list TILEFREE TILEOBS TILEFREE TILEOBS TILEFREE TILEFREE
                         TILEFREE TILEFREE)))
(define MAP6 (list (list TILEFREE TILEOBS TILEFREE TILEFREE TILEFREE TILEOBS
                         TILEFREE TILEOBS TILEFREE TILEFREE TILEFREE TILEOBS)
                   (list TILEOBS TILEOBS TILEFREE TILEOBS TILEFREE TILEFREE
                         TILEFREE TILEFREE TILEFREE TILEOBS TILEOBS TILEFREE)
                   (list TILEFREE TILEFREE TILEFREE TILEOBS TILEOBS TILEFREE
                         TILEFREE TILEOBS TILEFREE TILEFREE TILEFREE TILEOBS)
                   (list TILEFREE TILEOBS TILEFREE TILEOBS TILEFREE TILEOBS
                         TILEOBS TILEOBS TILEFREE TILEFREE TILEOBS TILEOBS)
                   (list TILEFREE TILEFREE TILEFREE TILEOBS TILEFREE TILEOBS
                         TILEFREE TILEOBS TILEFREE TILEOBS TILEFREE TILEOBS)
                   (list TILEFREE TILEOBS TILEFREE TILEFREE TILEFREE TILEOBS
                         TILEFREE TILEOBS TILEFREE TILEFREE TILEFREE TILEOBS)
                   (list TILEOBS TILEOBS TILEFREE TILEOBS TILEFREE TILEFREE
                         TILEFREE TILEFREE TILEFREE TILEOBS TILEOBS TILEFREE)
                   (list TILEFREE TILEFREE TILEFREE TILEOBS TILEOBS TILEFREE
                         TILEFREE TILEOBS TILEFREE TILEFREE TILEFREE TILEOBS)
                   (list TILEFREE TILEOBS TILEFREE TILEOBS TILEFREE TILEOBS
                         TILEOBS TILEOBS TILEFREE TILEFREE TILEOBS TILEOBS)
                   (list TILEFREE TILEFREE TILEFREE TILEOBS TILEFREE TILEOBS
                         TILEFREE TILEOBS TILEFREE TILEOBS TILEFREE TILEOBS)
                   (list TILEFREE TILEFREE TILEFREE TILEOBS TILEFREE TILEOBS
                         TILEFREE TILEOBS TILEFREE TILEOBS TILEFREE TILEOBS)
                   (list TILEFREE TILEOBS TILEFREE TILEFREE TILEFREE TILEOBS
                         TILEFREE TILEOBS TILEFREE TILEFREE TILEFREE TILEOBS)))

#;
(define (fn-for-map map)
  (local [(define (fn-for-lolot lolot)
            (cond [(empty? lolot) (...)]
                  [else (... (fn-for-lot (first lolot))
                             (fn-for-lolot (rest lolot)))]))
          (define (fn-for-lot lot)
            (cond [(empty? lot) (...)]
                  [else (... (fn-for-t (first lot))
                             (fn-for-lot (rest lot)))]))]
    (fn-for-lolot map)))

(@htdd SearchState)
(define-struct ss (map pos-wl path-wl visited))
;; SearchState is (make-ss Map (list Pos) (list (list Pos)) (list Pos))
;; interp. map is the map on which the search is being conducted,
;;         pos-wl is a worklist of positions to visited,
;;         path is a tandem worklist with paths to the corresponding positions,
;;         visited is a visited accumulator.
;; CONSTRAINTS:
;; - pos-wl and path-wl are always of the same length.
;; - All paths in path-wl are reversed.
;; - All positions in any list of path-wl are in visited.
;; - All positions in pos-wl are valid positions in map (i.e. they represent a
;;   position within the bounds of map, they do not represent positions with
;;   obstacles in map, they are not in any list of path-wl, and they are not in
;;   visited).
;; - All positions in all path-wl paths and in visited were valid positions in
;;   map at some point in time (i.e. they represent a position within the bounds
;;   of map and they do not represent positions with obstacles).
(define SSE (make-ss MAPE empty empty empty))
(define SS0 (make-ss MAP0 empty empty empty))
(define SS1A (make-ss MAP1 (list (make-pos 0 0)) (list empty) empty))
(define SS1B (make-ss MAP1 empty empty (list (make-pos 0 0))))
(define SS2A (make-ss MAP2 (list (make-pos 1 1)) (list empty) empty))
(define SS2B (make-ss MAP2 (list (make-pos 1 0)) (list (list (make-pos 1 1)))
                      (list (make-pos 1 1))))
(define SS2C (make-ss MAP2 (list (make-pos 0 0)) (list (list (make-pos 1 0)
                                                             (make-pos 1 1)))
                      (list (make-pos 1 0) (make-pos 1 1))))
(define SS2D (make-ss MAP2 empty empty
                      (list (make-pos 0 0) (make-pos 1 0) (make-pos 1 1))))
(define SS3A (make-ss MAP3 (list (make-pos 0 0)) (list empty) empty))
(define SS3B (make-ss MAP3 (list (make-pos 1 0) (make-pos 0 1))
                      (list (list (make-pos 0 0)) (list (make-pos 0 0)))
                      (list (make-pos 0 0))))
(define SS3C (make-ss MAP3 (list (make-pos 1 1) (make-pos 0 1))
                      (list (list (make-pos 1 0) (make-pos 0 0))
                            (list (make-pos 0 0)))
                      (list (make-pos 1 0) (make-pos 0 0))))
(define SS3D (make-ss MAP3 (list (make-pos 0 1))
                      (list (list (make-pos 0 0)))
                      (list (make-pos 1 1) (make-pos 1 0) (make-pos 0 0))))
(define SS3E (make-ss MAP3 empty
                      empty
                      (list (make-pos 0 1) (make-pos 1 1) (make-pos 1 0)
                            (make-pos 0 0))))
(define SS4A (make-ss MAP4 (list (make-pos 0 0)) (list empty) empty))
(define SS4B (make-ss MAP4 (list (make-pos 1 0)
                                 (make-pos 0 1))
                      (list (list (make-pos 0 0)) (list (make-pos 0 0)))
                      (list (make-pos 0 0))))
(define SS5 (make-ss MAP5 (list (make-pos 2 0))
                     (list empty)
                     empty))
(define SS6 (make-ss MAP6 (list (make-pos 6 2))
                     (list empty)
                     empty))

#;
(define (fn-for-ss ss)
  (... (fn-for-map (ss-map ss))
       (pos-wl ss)
       (path-wl ss)
       (visited ss)))

;; ===================

(@htdw SearchState)

(@htdf main)
(@signature SearchState -> SearchState)
;; Start program with (main SS5) or (main SS6).
;; <no check-expects for main function>

(@template-origin htdw-main)

(define (main s)
  (big-bang s
    (to-draw render)
    (on-key key-handler)))


(@htdf key-handler)
(@signature SearchState KeyEvent -> SearchState)
;; Processes a key press from the user.
(check-expect (key-handler SS2B "n") SS2C)
(check-expect (key-handler SS4A "n") SS4B)
(check-expect (key-handler SS2A " ") SS2A)
(check-expect (key-handler SS2A "b") SS2A)
(check-expect (key-handler SS4A " ") SS4A)

(@template-origin KeyEvent)

(define (key-handler ss ke)
  (cond [(string=? ke "n") (next-state ss)]
        [else ss]))


(@htdf next-state)
(@signature SearchState -> SearchState)
;; Produces the next search state.
(check-expect (next-state SSE) SSE)
(check-expect (next-state SS0) SS0)
(check-expect (next-state SS1A) SS1B)
(check-expect (next-state SS2A) SS2B)
(check-expect (next-state SS2B) SS2C)
(check-expect (next-state SS2C) SS2D)
(check-expect (next-state SS4A) SS4B)

(define (next-state ss)
  (local [(define n (length (ss-map ss)))
          (define (is-obstacle? t)
            (cond [(string=? t "obstacle") true]
                  [else false]))
          (define (get-coords--lot lot x)
            (cond [(zero? x) (first lot)]
                  [else (get-coords--lot (rest lot) (sub1 x))]))
          (define (get-coords--lolot lolot x y)
            (cond [(zero? y) (get-coords--lot (first lolot) x)]
                  [else (get-coords--lolot (rest lolot) x (sub1 y))]))
          (define (valid-next-pos? pos)
            (and (not (pos-in-lop? pos (ss-visited ss)))
                 (not (pos-in-lop? pos (ss-pos-wl ss)))
                 (>= (pos-x pos) 0)
                 (< (pos-x pos) n)
                 (>= (pos-y pos) 0)
                 (< (pos-y pos) n)
                 (not (is-obstacle? (get-coords--lolot (ss-map ss)
                                                       (pos-x pos)
                                                       (pos-y pos))))))]
    (cond [(empty? (ss-pos-wl ss)) ss]
          [else (local [(define pos (first (ss-pos-wl ss)))
                        (define path (first (ss-path-wl ss)))
                        (define x (pos-x pos))
                        (define y (pos-y pos))
                        (define valid-next-lop
                          (filter valid-next-pos?
                                  (list (make-pos (add1 x) y)
                                        (make-pos x (add1 y))
                                        (make-pos (sub1 x) y)
                                        (make-pos x (sub1 y)))))]
                  (make-ss (ss-map ss)
                           (append valid-next-lop (rest (ss-pos-wl ss)))
                           (append (make-list (length valid-next-lop)
                                              (cons pos path))
                                   (rest (ss-path-wl ss)))
                           (cons pos (ss-visited ss))))])))


(@htdf render)
(@signature SearchState -> Image)
;; Produces the graphical representation of a SearchState.
(check-expect (render SSE) (square 0 "outline" "black"))
(check-expect (render SS0)
              (overlay (square SQUARE-SIDE "outline" "black")
                       (square SQUARE-SIDE "solid" OBSTACLE-COLOR)))
(check-expect (render SS1A)
              (overlay (square SQUARE-SIDE "outline" "black")
                       (square SQUARE-SIDE "solid" NEXT-POS-COLOR)))
(check-expect (render SS1B)
              (overlay (square SQUARE-SIDE "outline" "black")
                       (square SQUARE-SIDE "solid" VISITED-COLOR)))
(check-expect (render SS2B)
              (overlay
               (square (* 2 SQUARE-SIDE) "outline" "black")
               (above (beside (square SQUARE-SIDE "solid" FREE-COLOR)
                              (square SQUARE-SIDE "solid" NEXT-POS-COLOR))
                      (beside (square SQUARE-SIDE "solid" OBSTACLE-COLOR)
                              (square SQUARE-SIDE "solid" NEXT-PATH-COLOR)))))
(check-expect (render SS3B)
              (overlay
               (square (* 2 SQUARE-SIDE) "outline" "black")
               (above (beside (square SQUARE-SIDE "solid" NEXT-PATH-COLOR)
                              (square SQUARE-SIDE "solid" NEXT-POS-COLOR))
                      (beside (square SQUARE-SIDE "solid" WORKLIST-COLOR)
                              (square SQUARE-SIDE "solid" FREE-COLOR)))))

(@template-origin encapsulated SearchState fn-composition)

(define (render ss)
  (local [(define (render--tile tile row-num col-num)
            (cond [(and (not (empty? (ss-pos-wl ss)))
                        (pos=? (make-pos col-num row-num)
                               (first (ss-pos-wl ss))))
                   (square SQUARE-SIDE "solid" NEXT-POS-COLOR)]
                  [(and (not (empty? (ss-path-wl ss)))
                        (pos-in-lop? (make-pos col-num row-num)
                                     (first (ss-path-wl ss))))
                   (square SQUARE-SIDE "solid" NEXT-PATH-COLOR)]
                  [(pos-in-lop? (make-pos col-num row-num)
                                (ss-pos-wl ss))
                   (square SQUARE-SIDE "solid" WORKLIST-COLOR)]
                  [(pos-in-lop? (make-pos col-num row-num)
                                (ss-visited ss))
                   (square SQUARE-SIDE "solid" VISITED-COLOR)]
                  [(string=? tile "obstacle")
                   (square SQUARE-SIDE "solid" OBSTACLE-COLOR)]
                  [else (square SQUARE-SIDE "solid" FREE-COLOR)]))
          (define (render--lot lot row-num col-num)
            (cond [(empty? lot) empty-image]
                  [else
                   (beside (render--tile (first lot) row-num col-num)
                           (render--lot (rest lot) row-num (add1 col-num)))]))
          (define (render--lolot lolot row-num)
            (cond [(empty? lolot) empty-image]
                  [else (above (render--lot (first lolot) row-num 0)
                               (render--lolot (rest lolot) (add1 row-num)))]))
          (define n (length (ss-map ss)))]
    (overlay (square (* SQUARE-SIDE n) "outline" "black")
             (render--lolot (ss-map ss) 0))))

(@htdf pos=?)
(@signature Position Position -> Boolean)
;; Produces true if two positions are equal, false otherwise.
(check-expect (pos=? (make-pos 0 0) (make-pos 0 0)) true)
(check-expect (pos=? (make-pos 2 3) (make-pos 2 3)) true)
(check-expect (pos=? (make-pos 4 1) (make-pos 4 1)) true)
(check-expect (pos=? (make-pos 2 3) (make-pos 4 1)) false)
(check-expect (pos=? (make-pos 4 1) (make-pos 1 4)) false)
(check-expect (pos=? (make-pos 4 1) (make-pos 4 4)) false)

(@template-origin Position)

(define (pos=? p1 p2)
  (and (= (pos-x p1) (pos-x p2))
       (= (pos-y p1) (pos-y p2))))


(@htdf pos-in-lop?)
(@signature Position (listof Position) -> Boolean)
;; Produce true if given pos is in given lop, comparing by pos=?.
(check-expect (pos-in-lop? (make-pos 0 0) empty) false)
(check-expect (pos-in-lop? (make-pos 0 0) (list (make-pos 0 0))) true)
(check-expect (pos-in-lop? (make-pos 0 0) (list (make-pos 2 3))) false)
(check-expect (pos-in-lop? (make-pos 0 0)
                           (list (make-pos 2 3) (make-pos 4 1) (make-pos 0 0)))
              true)
(check-expect (pos-in-lop? (make-pos 4 1)
                           (list (make-pos 2 3) (make-pos 4 1) (make-pos 0 0)))
              true)
(check-expect (pos-in-lop? (make-pos 0 0)
                           (list (make-pos 2 3) (make-pos 4 1) (make-pos 2 4)))
              false)
(check-expect (pos-in-lop? (make-pos 3 2)
                           (list (make-pos 2 3) (make-pos 4 1) (make-pos 0 0)))
              false)

(@template-origin use-abstract-fn)

(define (pos-in-lop? p1 lop)
  (ormap (lambda (p2) (pos=? p1 p2)) lop))
