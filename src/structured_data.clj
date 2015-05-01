(ns structured-data)
; Muista instarepl!!!

; EXERCISE 1
(defn do-a-thing [x]
  (let [xx (+ x x)]
  (Math/pow xx xx))
  )


; EXERCISE 2
(defn spiff [v]
  (+ (get v 0) (get v 2) )
  )


; EXERCISE 3
(defn cutify [v]
  (conj v "<3" )
  )


; EXERCISE 4
(defn spiff-destructuring [v]
  (let [ [x y z] v ] (+ x z))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])


; EXERCISE 5
(defn width [rectangle]
  (let [ [a b] rectangle
         [x1 y1] a
         [x2 y2]  b
         ]
    (if (< x1 x2) (- x2 x1) (- x1 x2) )
  )
  )

(defn height [rectangle]
  (let [ [a b] rectangle
         [x1 y1] a
         [x2 y2]  b
         ]

  (if (< y1 y2) (- y2 y1) (- y1 y2) )
  )
  )


; EXERCISE 6
(defn square? [rectangle]
  (= (height rectangle) (width rectangle) )
  )


; EXERCISE 7
(defn area [rectangle]
  (* (height rectangle) (width rectangle) )
  )

; (area (rectangle [3 1] [10 4]))


; EXERCISE 8
(defn contains-point? [rectangle point]
  (let [ [a b] rectangle
         [x1 y1 ] a
         [x2 y2]  b
         [xp yp]  point
         ]
  (and (or (<= x1 xp x2) (>= x1 xp x2))  (or (<= y1 yp y2) (>= y1 yp y2)) )
  ) )

;(contains-point? (rectangle [0 0] [2 2])
;                 (point 1 1))            ;=> true
;(contains-point? (rectangle [0 0] [2 2])
;                 (point 2 1))            ;=> true
;(contains-point? (rectangle [0 0] [2 2])
;                 (point -3 1))           ;=> false
;(contains-point? (rectangle [0 0] [2 2])
;                 (point 1 3))            ;=> false
;(contains-point? (rectangle [1 1] [2 2])
;                 (point 1 1))            ;=> true
;(contains-point? (rectangle [1 1] [1 1])
;                 (point 1 1))  ;=> true


; EXERCISE 9
(defn contains-rectangle? [outer inner]
  (let [ [a b] inner
         ]
    (and (contains-point? outer a) (contains-point? outer b) )
    ))

;(contains-rectangle? (rectangle [0 0] [3 3])
;                     (rectangle [1 1] [2 2])) ;=> true
;(contains-rectangle? (rectangle [0 0] [2 2])
;                     (rectangle [1 1] [3 3])) ;=> false
;(contains-rectangle? (rectangle [0 0] [1 1])
;                     (rectangle [0 0] [1 1])) ;=> true
;(contains-rectangle? (rectangle [0 0] [1 1])
;                     (rectangle [1 1] [2 2])) ;=> false


; EXERCISE 10
(defn title-length [book]
  (count (:title book) ) )


; EXERCISE 11
(defn author-count [book]
  (count (:authors book) ) )


; EXERCISE 12
(defn multiple-authors? [book]
  (not (= 1 (author-count book)) ) )


; EXERCISE 13
(defn add-author [book new-author]
  (let [
        authh (:authors book)
        nauthh (conj authh new-author)
        ]
    (assoc book :authors nauthh)
       )
  )


; EXERCISE 14
(defn alive? [author]
  (not (contains? author :death-year) )
  )


; EXERCISE 15
(defn element-lengths [collection]
  (map count collection )
  )

;(element-lengths ["foo" "bar" "" "quux"])  ;=> (3 3 0 4)
;(element-lengths ["x" [:a :b :c] {:y 42}]) ;=> (1 3 1)


; EXERCISE 16
(defn second-elements [collection]
  (map (fn mmm [x] (get x 1)) collection )
  )

;(second-elements [[1 2] [2 3] [3 4]]) ;=> (2 3 4)
;(second-elements [[1 2 3 4] [1] ["a" "s" "d" "f"]]) ;=> (2 nil "s")


; EXERCISE 17
(defn titles [books]
  (map :title books )
  )


; EXERCISE 19
(defn monotonic? [a-seq]
  (or (apply <= a-seq )
      (apply >= a-seq )
      )
  )

;(monotonic? [1 2 3])     ;=> true
;(monotonic? [0 1 10 11]) ;=> true
;(monotonic? [3 2 0 -3])  ;=> true
;(monotonic? [3 2 2])     ;=> true    Not strictly monotonic
;(monotonic? [1 2 1 0])   ;=> false


; EXERCISE 18
(defn stars [n]
  (apply str ( repeat n "*" )   )
    )


; EXERCISE 20
(defn toggle [a-set elem]
  (if (contains? a-set elem )
    (disj a-set elem )
    (conj a-set elem )
    )
  )

;(toggle #{:a :b :c} :d) ;=> #{:a :c :b :d}
;(toggle #{:a :b :c} :a) ;=> #{:c :b}


; EXERCISE 21
(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq))))
  )

;(contains-duplicates? [1 1 2 3 -40]) ;=> true
;(contains-duplicates? [1 2 3 -40]) ;=> false
;(contains-duplicates? [1 2 3 "a" "a"]) ;=> true


; EXERCISE 22
(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book ) ) )
  )


; EXERCISE 23
(defn has-author? [book author]
  (contains? (:authors book) author)
  )


; EXERCISE 24
(defn authors [books]
  (set (apply concat (map :authors books )))
  )


; EXERCISE 25
(defn all-author-names [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books))))
  )


; EXERCISE 26
(defn author->string [author]
  ( let [ aname (:name author)
          years ( if (nil? (:birth-year author) ) ""
                  (if (nil? (:death-year author))  (str " (" (:birth-year author) " - )")
                    (str " (" (:birth-year author) " - " (:death-year author) ")") ))
          ]
    (str aname years ) )
  )


; EXERCISE 27
(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors) ))
  )


; EXERCISE 28
(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book) ) )
  )


; EXERCISE 29
(defn books->string [books]
  (let [ intro (if (= 1 (count books)) "1 book. " (if (= 0 (count books)) "No books" (str (count books) " books. " ))  ) ]
  (str intro (apply str (interpose ". " (map book->string books) )) "." ))
  )


; EXERCISE 30
(defn books-by-author [author books]
  (filter (fn [x] ( has-author?  x author )) books )
  )


; EXERCISE 31
(defn author-by-name [name authors]
  ( let [
  alist (filter (fn [x] ( = (:name x) name )) authors)
   ]  ; (cond  (= (count alist) 0 ) nil   :else (first alist) )
    (first alist)    )
  )


; EXERCISE 32
(defn living-authors [authors]
  (filter (fn [x] ( nil? (:death-year x ) )) authors  )
  )


; EXERCISE 33
(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book) ) ))
  )


; EXERCISE 34
(defn books-by-living-authors [books]
  (filter has-a-living-author? books )
  )

; %________%
