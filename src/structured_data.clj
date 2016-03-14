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
  (not (= (count (book :authors)) 1))
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
  (or (apply >= a-seq) (apply <= a-seq))
  )

(defn stars [n]
  (apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem) (conj a-set elem)
  ))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq)))
  )

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
  )

;(def china {:name "China Miéville", :birth-year 1972})
;(def octavia {:name "Octavia E. Butler"
;              :birth-year 1947
;              :death-year 2006})
;(def friedman {:name "Daniel Friedman" :birth-year 1944})
;(def felleisen {:name "Matthias Felleisen"})

;(def cities {:title "The City and the City" :authors #{china}})
;(def wild-seed {:title "Wild Seed", :authors #{octavia}})
;(def embassytown {:title "Embassytown", :authors #{china}})
;(def little-schemer {:title "The Little Schemer"
;                     :authors #{friedman, felleisen}})

;(def books [cities, wild-seed, embassytown, little-schemer])

(defn has-author? [book author]
  (contains? (:authors book) author)
  )

(defn authors [books]
  (let [get-authors (fn [x] (:authors x))]
    (apply clojure.set/union (map get-authors books))
  ))

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (let [years (fn [x] (str "("(:birth-year x)" - "(:death-year x)")"))]
    (if (contains? author :birth-year) (str (:name author)" "(years author)) (:name author))
    )
  )

(defn authors->string [authors]
  (let [auth-str (fn [x] (author->string x))]
    (apply str(interpose ", "(map auth-str authors)))
  ))

(defn book->string [book]
  (str (:title book) ", written by " (apply str (interpose ", " (map author->string (:authors book)))))
  )

(defn books->string [books]
 (let [strings (fn [x] (book->string x))]
  (cond
   (empty? books)
    "No books."
   (= (count books) 1)
   (str (count books) " book. " (apply str (interpose ". " (map strings books))) ".")
    :else (str (count books) " books. " (apply str (interpose ". " (map strings books))) ".")
  )))

(defn books-by-author [author books]
  (let [func (fn [x] (contains? (:authors x) author))]
  (filter func books)
  ))

;(def authors #{china, felleisen, octavia, friedman})

(defn author-by-name [name authors]
  (first (filter (fn [authors] (= (:name authors) name)) authors))
  )

(defn living-authors [authors]
  (filter alive? authors)
  )

;(def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
;(def christopher {:name "Christopher Tolkien" :birth-year 1924})
;(def kay {:name "Guy Gavriel Kay" :birth-year 1954})

;(def silmarillion {:title "Silmarillion"
;                   :authors #{jrrtolkien, christopher, kay}})

;(def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
;(def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})

;(def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book))))
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

; %________%
