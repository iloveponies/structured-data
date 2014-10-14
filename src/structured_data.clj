(ns structured-data)

(defn do-a-thing [x]

  (let [xx (+ x x)]
    (Math/pow xx xx)
  )
  )

(defn spiff [v]
  (if ( <= 3 (count v))
  (+ (get v 0) (get v 2))
  "?"
  )
)

(defn cutify [v]

  (conj v "<3")

  )


(defn spiff-destructuring [v]

  (if ( <= 3 (count v))
  (let [[x y z] v]
    (+ x z))
  "?"
  )

  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]

  (

   let

   [[ [x1 y1]  [x2 y2]] rectangle]
     (- x2 x1)
   )


  )

(defn height [rectangle]

  (let [[[x1 y1] [x2 y2]] rectangle]
      (- y2 y1)
   )

  )

(width (rectangle [1 1] [1 1]))

(defn square? [rectangle]

  ( == (width rectangle) (height rectangle))

  )


(defn area [rectangle]

  (* (width rectangle) (height rectangle))

  )

(defn contains-point? [rectangle point]

  (let

    [[[x1 y1] [x2 y2]] rectangle
     [x3 y3] point
     ]

    (and (and (>= x3 x1) (<= x3 x2))
         (and (>= y3 y1) (<= y3 y2))
         )

    )

  )

(defn contains-rectangle? [outer inner]

    (let

      [[p1 p2] inner]

      (and (contains-point? outer p1) (contains-point? outer p2) )

      )

  )


(defn title-length [book]

  (count (:title book))

  )


(defn author-count [book]

  (count (:authors book))

  )

(defn multiple-authors? [book]

  (< 1 (author-count book))

  )



(multiple-authors? cities)         ;=> false
(multiple-authors? wild-seed)      ;=> false
(multiple-authors? little-schemer) ;=> true

(defn add-author [book new-author]

  (

       let
       [

        authors (conj (:authors book) new-author)]


       (assoc book :authors authors)

   )

  )


(defn alive? [author]

  (not (contains? author :death-year))

  )


(defn element-lengths [collection]

  (map count collection)

  )


(defn second-elements [collection]

  (map (fn [col] (get col 1)) collection)

  )

(def china {:name "China Miéville", :birth-year 1972})
(def octavia {:name "Octavia E. Butler"
              :birth-year 1947
              :death-year 2006})
(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})

(def cities {:title "The City and the City" :authors [china]})
(def wild-seed {:title "Wild Seed", :authors [octavia]})
(def embassytown {:title "Embassytown", :authors [china]})
(def little-schemer {:title "The Little Schemer"
                     :authors [friedman, felleisen]})

(def books [cities, wild-seed, embassytown, little-schemer])

(defn titles [books]

  (map :title books)

  )


(defn monotonic? [a-seq]

  (or (apply <= a-seq) (apply >= a-seq))

  )



(defn stars [n]


  (apply str (repeat 5 "*"))

  )

(defn toggle [a-set elem]
  :-)

(defn contains-duplicates? [a-seq]
  :-)

(defn old-book->new-book [book]
  :-)

(defn has-author? [book author]
  :-)

(defn authors [books]
  :-)

(defn all-author-names [books]
  :-)

(defn author->string [author]
  :-)

(defn authors->string [authors]
  :-)

(defn book->string [book]
  :-)

(defn books->string [books]
  :-)

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
