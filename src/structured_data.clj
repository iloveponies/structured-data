(ns structured-data)

(defn do-a-thing [x]
  (let [a (+ x x)]
    (Math/pow a a)
    )
  )

(defn spiff [v]
  (+ (first v) (get v 2))
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[a b c] [(first v) :q (get v 2)]]
    (+ a c)
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
    (= (- x2 x1) (- y2 y1))
    )
  )

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- y2 y1) (- x2 x1)))
    )

(defn contains-point? [rectanglex point]
  (let [[[x1 y1] [x2 y2]] rectanglex [x3 y3] point]
    (and (<= x1 x3 x2) (<= y1 y3 y2)))
  )

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer [[x3 y3] [x4 y4]] inner]
    (and
      (contains-point? (rectangle [x1 y1] [x2 y2]) (point x3 y3))
      (contains-point? (rectangle [x1 y1] [x2 y2]) (point x4 y4)))
  ))

;(def china {:name "China Miéville", :birth-year 1972})
;(def octavia {:name "Octavia E. Butler":birth-year 1947 :death-year 2006})
;(def friedman {:name "Daniel Friedman" :birth-year 1944})
;(def felleisen {:name "Matthias Felleisen"})

;(def cities {:title "The City and the City" :authors [china]})
;(def wild-seed {:title "Wild Seed", :authors [octavia]})
;(def embassytown {:title "Embassytown", :authors [china]})
;(def little-schemer {:title "The Little Schemer" :authors [friedman, felleisen]})
;(def books [cities, wild-seed, embassytown, little-schemer])

(defn title-length [book]
  (count (book :title))
  )

(defn author-count [book]
  (count (book :authors)))

(defn multiple-authors? [book]
  (if (not (= (count (book :authors)) 1)) true false)
  )

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author))
  )

(defn alive? [author]
  (not (contains? author :death-year))
  )

(defn element-lengths [collection]
  (map (fn [x] (count x)) collection)
  )

;(defn second-elements [collection]
;  (map (fn [x] (get x 1)) collection)
;  )
; exercise requires let so...

(defn second-elements [collection]
  (let [get-second (fn [x] (get x 1))] (map get-second collection)))

(defn titles [books]
  (map :title books)
  )
;; pregunta!
;; (titles books) funciona
;; (titles cities) no funciona
;; (title [citites]) funciona
;; veo que books es un vector y cities un mapa
;; la pregunta seria, por que un mapa no puede ser parametro?

(defn monotonic? [a-seq]
  :-)

(defn stars [n]
  (apply str (repeat n "*"))
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
