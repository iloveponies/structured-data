(ns structured-data)



(defn do-a-thing [x]
  (let [y (+ x x)]
  (Math/pow y y)) )

(defn spiff [v] ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (> (count v) 2) (+ (get v 0) (get v 2) ) \? ) )

(defn cutify [v] (conj v "<3") )


(defn spiff-destructuring [v]
(let [[x y z] v]
    (if (> (count v) 2)
    (+ x z)
   '?
      )
   )
 )



(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
 (let [[[x1 y1] [x2 y2]] rectangle]
  (- x2 x1)
  )
)


(defn height [rectangle]
   (let [[[x1 y1] [x2 y2]] rectangle]
  (- y2 y1)
   )
  )


(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (if (== (height rectangle) ( width rectangle) ) true false )

    )
  )


(defn area [rectangle]
(* (height rectangle) ( width rectangle))

  )


(defn contains-point? [rectangle point]
 (let [[[x1 y1] [x2 y2]] rectangle
    [p1 p2] point ]
  (if (and (<= x1 p1 x2 ) (<= y1 p2 y2  ) ) true false  )


  )

)


(defn contains-rectangle? [outer inner]
(let [ [[x1 y1] [x2 y2]] outer
    [[p1 p2] [z1 z2]] inner ]

    (if (and ( contains-point? (rectangle [x1 y1] [x2 y2]) (point p1 p2)  )
             ( contains-point? (rectangle [x1 y1] [x2 y2]) (point z1 z2) )
             ) true false )
  )
)




(defn title-length [book] ;;;;;;;;;;;;;;;;;;;;;??? norm
  (count (get book :title) )
  )

(defn author-count [book]
  (count (get book :authors))
  )

(defn multiple-authors? [book]
   (if ( == (author-count book) 1) false true)
  )


(defn add-author [book new-author]  ;;;;;;;;;???? not working


(let [authors (:authors book)]

(assoc book :authors (conj authors new-author)))

)


(defn alive? [author]
  (if (= (get author :death-year) nil) true false )
)


(defn munge1 [x]
( count x)
)

(defn element-lengths [collection]

  (map munge1 collection)
)

(defn second-elements [collection]
  ( let [abc (fn [x] (get x 1 ))]
  (map abc collection)
  )
)


(defn titles [books]

 (let[ people1 books]
 (map :title people1)
 )
)

(defn monotonic? [a-seq]
   (if (or (apply <= a-seq) (apply >= a-seq) ) true false  )
  )

(defn stars [n]
(apply str (repeat n "*"))
  )


(defn toggle [a-set elem]
   ( if  (contains? a-set elem ) (disj a-set elem) (conj a-set elem)
  )
)

(defn contains-duplicates? [a-seq]
 (let [cou (count a-seq)
        cou1 (count ( set a-seq))
        ]
    (if (== cou cou1) false true))
)


(defn old-book->new-book [book]
   (let [abc ( :authors book)
       abc1 (set abc)  ]
   (assoc book :authors abc1)

   )
)


(defn has-author? [book author]

    (let [abc ( :authors book)
       abc1 (set abc) ]
  (if (contains? abc1 author) true false)
  )
)

(defn authors [books]


  (apply clojure.set/union (map (fn [book] (:authors book)) books))
)

(defn all-author-names [books]


  (set (map (fn [author] (:name author)) (authors books)) )

)

(defn author->string [author]
  (str (:name author)
      (if (contains? author :birth-year)
         (str " (" (:birth-year author) " - " (:death-year author) ")")))
  )

(defn authors->string [authors]
   (apply str (interpose ", "  (map (fn[author] (author->string  author )) authors) )   )

  )

(defn book->string [book]

   (apply str  [ (:title book) ", written by " (authors->string (:authors book)  )  ] )
)

(defn books->string [books]
    (let[ a   (apply str (map (fn[book] ( str (book->string  book ) ". ") ) books)     )
          b   (apply str (map (fn[book] ( str (book->string  book ) ".") ) books)     )
          ]

(cond
  (  if (== (count  books ) 0  ) true false   ) "No books."

  (  if (= (count  books ) 1 ) true  false ) (str (count  books ) " book. "  b  )
  :else        (str (count  books ) " books. "  a  ))

  )

)


(defn books-by-author [author books]
 (filter #(has-author? % author)  books)

)


(defn author-by-name [name authors]
     (first (filter #(= name (:name %)) authors)))





(defn living-authors [authors]

   (filter   (fn [author]  (alive? author))  authors )
  )

(defn has-a-living-author? [book]
  ( not (empty? (filter   (fn [author]  (alive? author))  (:authors book) ) ) )
)

(defn books-by-living-authors [books]
  (filter has-a-living-author?  books)
  )

; %________%
