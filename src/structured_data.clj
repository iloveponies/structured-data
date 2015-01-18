(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)
   )
  )

(do-a-thing 2)
()

(defn spiff [v]
  (+ (get v 0) (get v 2))
  )

(spiff [1 2 3])
(defn cutify [v]
  (conj v "<3")
  )
(cutify [1])

(defn spiff-destructuring [v]
  (let [[x z y]v]
    (+ x y)
  )
  )
(spiff-destructuring [1 2 3])

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
    )
  )
9
(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
    )
  )
(height (rectangle [1 1] [5 1])) ; 0
(height (rectangle [1 1] [5 5])) ; 4
(height (rectangle [0 0] [2 3])) ; 3

(width (rectangle [1 1] [5 1]))  ; 4
(width (rectangle [1 1] [1 1]))  ; 0
(width (rectangle [3 1] [10 4])) ; 7

(defn square? [rec]
  (if (= (height rec) (width rec)) true false)
  )
(square? (rectangle [1 1] [2 2])) ;=> true
(square? (rectangle [1 1] [2 3])) ;=> false
(square? (rectangle [1 1] [1 1])) ;=> true
(square? (rectangle [3 2] [1 0])) ;=> true
(square? (rectangle [3 2] [1 1])) ;=> false

(defn area [rectangle]
  (* (height rectangle) (width rectangle))
  )
(area (rectangle [1 1] [5 1]))  ;=> 0
(area (rectangle [0 0] [1 1]))  ;=> 1
(area (rectangle [0 0] [4 3]))  ;=> 12
(area (rectangle [3 1] [10 4])) ;=> 21

(defn contains-point? [rectangle point]
  (let [[x y] point]
    (let [[[x1 y1] [x2 y2]] rectangle]
      (and
       (<= x1 x x2)
       (<= y1 y y2)
       )
      )
    )
  )
(contains-point? (rectangle [0 0] [2 2])
                 (point 1 1))            ;=> true
(contains-point? (rectangle [0 0] [2 2])
                 (point 2 1))            ;=> true
(contains-point? (rectangle [0 0] [2 2])
                 (point -3 1))           ;=> false
(contains-point? (rectangle [0 0] [2 2])
                 (point 1 3))            ;=> false
(contains-point? (rectangle [1 1] [2 2])
                 (point 1 1))            ;=> true
(contains-point? (rectangle [1 1] [1 1])
                 (point 1 1))            ;=> true

(defn contains-rectangle? [outer inner]
  (let [[i1 i2] inner]
    (and
     (contains-point? outer i1)
     (contains-point? outer i2)
     )
    )
  )
(contains-rectangle? (rectangle [0 0] [3 3])
                     (rectangle [1 1] [2 2])) ;=> true
(contains-rectangle? (rectangle [0 0] [2 2])
                     (rectangle [1 1] [3 3])) ;=> false
(contains-rectangle? (rectangle [0 0] [1 1])
                     (rectangle [0 0] [1 1])) ;=> true
(contains-rectangle? (rectangle [0 0] [1 1])
                     (rectangle [1 1] [2 2])) ;=> false


(defn title-length [book]
  (count (:title book))
  )
(defn author-count [book]
   (count (:authors book))
  )


(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false)
  )


(defn add-author [book new-author]
    (assoc book :authors (conj (get book :authors) new-author))
  )


(defn alive? [author]
  (not (contains? author :death-year))
  )


(defn element-lengths [collection]
  (map count collection)
  )

(element-lengths ["foo" "bar" "" "quux"])  ;=> (3 3 0 4)
(element-lengths ["x" [:a :b :c] {:y 42}]) ;=> (1 3 1)

(defn second-elements [collection]
  (let[get1 (fn [collection] (get collection 1))]
    (map get1 collection)
    )
  )

(second-elements [[1 2] [2 3] [3 4]]) ;=> (2 3 4)
(second-elements [[1 2 3 4] [1] ["a" "s" "d" "f"]])
;=> (2 nil "s")

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq ) (apply >= a-seq))
  )

(monotonic? [1 2 3])     ;=> true
(monotonic? [0 1 10 11]) ;=> true
(monotonic? [3 2 0 -3])  ;=> true
(monotonic? [3 2 2])     ;=> true    Not strictly monotonic
(monotonic? [1 2 1 0])   ;=> false

(defn stars [n]
  (apply str (repeat n "*"))
  )

(stars 2) ;=> "*"
(stars 7) ;=> "*******"
(stars 21) ;=> "***"

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )
  )


(defn contains-duplicates? [a-seq]
  (if (= (count (set a-seq)) (count a-seq)) false true)
  )

(contains-duplicates? [1 1 2 3 -40]) ;=> true
(contains-duplicates? [1 2 3 -40]) ;=> false
(contains-duplicates? [1 2 3 "a" "a"]) ;=> true

(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors )))
  )


(defn has-author? [book author]
  (contains? (:authors book) author)
  )


(defn authors [books]
  (apply clojure.set/union (map :authors books))
  )


(defn all-author-names [books]
   (set (map :name (authors books)))
  )

(defn author->string [author]
   (apply str (:name author) (if (contains? author :death-year)
                             (str " (" (:birth-year author) " - " (:death-year author) ")")
                             (if (contains? author :birth-year)
                             (str " (" (:birth-year author) " - " ")")
                         )
        )
  )
  )
(defn authors->string [authors]
 (apply str  (interpose ", " (map author->string authors))))


(defn book->string [book]
  (apply str (:title book) ", written by " (authors->string (:authors book))))



(defn books->string [books]
  (if (= (count books) 0)
    (str "No books.")
    (str (apply str
           (count books)
           (if (= (count books) 1) " book. " " books. ")
           (interpose ". " (map book->string books))
          ) (str "."))
  )
)

(defn books-by-author [author books]
  (filter (fn[x] (has-author? x author)) books)
  )

(defn author-by-name [name authors]
   (if (= (count (filter (fn [x] (= (:name x) name)) authors)) 0)
     nil
     (first (filter (fn [x] (= (:name x) name)) authors))
  )
)

(defn living-authors [authors]
  (filter alive? authors)
  )


(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
)

; %________%
