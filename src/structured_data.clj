(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)
 )
 )

(defn spiff [v]
  (+ (get v 0) (get v 2)))


(defn cutify [v]
  (conj v "<3"))


(defn spiff-destructuring [v]
   (let [[x y z] v]
     (+ x z)
    )
  )


(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
    ))

(defn height [rectangle]
(let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
    ))



(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))


(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (if (and (<= x1 x x2) (<= y1 y y2))
      true
      false)
    )
  )


(defn contains-rectangle? [outer inner]
  (let [[lower-left upper-right] inner]
    (if (and(contains-point? outer lower-left) (contains-point? outer upper-right))
      true
      false
    ))
  )




(defn title-length [book]
  (count (:title book)))



(defn author-count [book]
  (count (:authors book)))



(defn multiple-authors? [book]
  ( > (author-count book) 1))

(defn add-author [book new-author]
  (let [new-authors (conj (:authors book) new-author)]
     (assoc book :authors new-authors)
    ))


(defn alive? [author]
  (not(contains? author :death-year)))

(defn element-lengths [collection]
  (map (fn [v] (count v)) collection))

(defn second-elements [collection]
      (let [get2 (fn [v] (get v 1))]
    (map get2 collection)
  )
  )



(defn titles [books]
  (map (fn [v] (:title v)) books)
  )


(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))


(defn stars [n]
  (apply str (repeat n "*")))


(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )
  )

(defn contains-duplicates? [a-seq]
  (< (count (set a-seq)) (count a-seq))
  )


(defn old-book->new-book [book]
  (let [new-authors (set (:authors book))]
    (assoc book :authors new-authors)
    )
  )

;; (def china {:name "China Miéville", :birth-year 1972})
;; (def octavia {:name "Octavia E. Butler"
;;               :birth-year 1947
;;               :death-year 2006})
;; (def friedman {:name "Daniel Friedman" :birth-year 1944})
;; (def felleisen {:name "Matthias Felleisen"})

;; (def cities {:title "The City and the City" :authors #{china}})
;; (def wild-seed {:title "Wild Seed", :authors #{octavia}})
;; (def embassytown {:title "Embassytown", :authors #{china}})
;; (def little-schemer {:title "The Little Schemer"
;;                      :authors #{friedman, felleisen}})

;; (def books [cities, wild-seed, embassytown, little-schemer])

(defn has-author? [book author]
   (contains? (:authors book) author)
  )

(defn authors [books]
  (apply clojure.set/union (map (fn [x] (:authors x)) books))
  )


(defn all-author-names [books]
  (let [aths (authors books)
        names (map (fn [author] (:name author)) aths)
        ]
        (set names)

  )
  )

(defn author->string [author]
   (let [str-year (if (contains? author :birth-year)
                    (str " (" (:birth-year author) " - " (:death-year author) ")")
                    ""
                    )
         ]
     (str (:name author) str-year)
     )
  )

(defn authors->string [authors]
  (apply str (interpose ", " (map (fn [author] (author->string author)) authors)))
  )

(defn book->string [book]
  (apply str (:title book) ", written by " (authors->string (:authors book)))
  ;(:name book)
  )

(defn books->string [books]
  (let [book-count (count books)
        books-desc (map (fn [book] (book->string book)) books)
        book-count-str (cond
                    (= book-count 0) "No books"
                    (= book-count 1) "1 book. "
                    :else (str book-count " books. ")
                    )]
    (str (apply str book-count-str (interpose ". " books-desc)) ".")
    )
  )



(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)
  )
;(def authors #{china, felleisen, octavia, friedman})

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors))
  )

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors)
  )

;; (def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
;; (def christopher {:name "Christopher Tolkien" :birth-year 1924})
;; (def kay {:name "Guy Gavriel Kay" :birth-year 1954})

;; (def silmarillion {:title "Silmarillion"
;;                    :authors #{jrrtolkien, christopher, kay}})

;; (def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
;; (def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})

;; (def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})


(defn has-a-living-author? [book]
  (if (empty? (living-authors (:authors book)))
    false
    true
    )
  )

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books)
  )

;=> (little-schemer cities embassytown silmarillion)
; %________%
