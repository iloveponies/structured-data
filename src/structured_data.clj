(ns structured-data)

(defn do-a-thing [x]
  (let [kx (+ x x) ]
  (Math/pow kx kx)
  ))

; (do-a-thing 2 )

(defn spiff [v]
  (+ (get v 0) (get v 2) )
  )

; (spiff [1 2 3])  ; ertsii pukkaa
; immutaabelit vektorit

(defn cutify [v]
  (conj v "<3" ) )

; (cutify [3])

(defn spiff-destructuring [v]
  (let [ [x y z] v ] (+ x z)) )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [ [a b] rectangle
         [x1 y1 ] a
         [x2 y2]  b
         ]

  (if (< x1 x2 ) (- x2 x1) (- x1 x2) )
  ))

; (width (rectangle [1 1] [1 5]) )

(defn height [rectangle]
  (let [ [a b] rectangle
         [x1 y1 ] a
         [x2 y2]  b
         ]

  (if (< y1 y2 ) (- y2 y1) (- y1 y2) )
  ))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle) ) )

(defn area [rectangle]
  (* (height rectangle) (width rectangle) ) )
; (area (rectangle [3 1] [10 4]))

(defn contains-point? [rectangle point]
  (let [ [a b] rectangle
         [x1 y1 ] a
         [x2 y2]  b
         [xp yp]  point
         ]

  (and (or (<= x1 xp x2 ) (>= x1 xp x2 ))  (or (<= y1 yp y2 ) (>= y1 yp y2 )) )
  ))


(defn contains-rectangle? [outer inner]
  (let [ [a b] inner
         ]
    (and (contains-point? outer a) (contains-point? outer b) )
    ))




(defn title-length [book]
  (count (:title book) ) )



(defn author-count [book]
 (count (:authors book) ) )

(defn multiple-authors? [book]
  (not (= 1 (author-count book)) ) )


(defn add-author [book new-author]
  (let [
        authh (:authors book)
        nauthh (conj authh new-author)
        ]
    (assoc book :authors nauthh)
       ))

(defn alive? [author]
  (not (contains? author :death-year) ) )

(defn element-lengths [collection]
  (map count collection ))


(defn second-elements [collection]
  (map (fn mmm [x] (get x 1)) collection ))

(defn titles [books]
  (map :title books ) )


; E18
(defn stars [n]
    (apply str ( repeat n "*" )   )
    )
; (concat ("*" "*"] )
; ( stars 3 )
; (str ( apply concat (stars 3) ) )
; (str (apply concat (repeat 5 "1" )))

(defn monotonic? [a-seq]
  (or (apply <= a-seq )
      (apply >= a-seq )
      )
  )


(defn toggle [a-set elem]
  (if (contains? a-set elem )
    (disj a-set elem )
    (conj a-set elem )
    )
  )

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq))))
  )


; E22 WIP? OK?

(defn old-book->new-book [book]
  ; ( let [ aunames (fn [bok] (map :authors))
  (assoc book :authors (set (:authors book ) ) )

  )


(defn has-author? [book author]
  (contains? (:authors book) author) )

(defn authors [books]
  (set (apply concat (map :authors books )))
  )

(defn all-author-names [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))


(defn author->string [author]
  ( let [ aname (:name author)
          years ( if (nil? (:birth-year author ) ) ""
                  (if (nil? (:death-year author ))  (str " (" (:birth-year author) " - )")
                    (str " (" (:birth-year author) " - " (:death-year author) ")") ))
          ]
    (str aname years )

  )
  )

; E27
(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors ) )) )

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book) ) ) )


(defn books->string [books]
  ( let [ intro (if (= 1 (count books)) "1 book. " (if (= 0 (count books)) "No books" (str (count books) " books. " ))  ) ]
  ( str intro (apply str (interpose ". " (map book->string books ) )) "." )
  )
  )

(defn books-by-author [author books]
  (filter (fn [x] ( has-author?  x author )) books ) )

; 31

(defn author-by-name [name authors]
  ( let [
  alist (filter (fn [x] ( = (:name x ) name )) authors )
   ]  ; (cond  (= (count alist) 0 ) nil   :else (first alist) )
    (first alist)
    )
)



(defn living-authors [authors]
  (filter (fn [x] ( nil? (:death-year x ) )) authors  )
  )

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book) ) )) )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books ) )

; %________%
