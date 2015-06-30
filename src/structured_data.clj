(ns structured-data)



;(def china {:name "China Miéville", :birth-year 1972})
;(def octavia {:name "Octavia E. Butler"
              ;:birth-year 1947
              ;:death-year 2006})
;(def friedman {:name "Daniel Friedman" :birth-year 1944})
;(def felleisen {:name "Matthias Felleisen"})


;(def cities {:title "The City and the City" :authors #{china}})
;(def wild-seed {:title "Wild Seed", :authors #{octavia}})
;(def embassytown {:title "Embassytown", :authors #{china}})
;(def little-schemer {:title "The Little Schemer"
                     ;:authors #{friedman, felleisen}})

;(def books [cities, wild-seed, embassytown, little-schemer])

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (let [a (get v 0)
        b (get v 2)]
    (+ a b)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- x1 x2))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y1 y2))))

(defn square? [rectangle]
  (if (= (width rectangle) (height rectangle)) true false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [px py] point]
    (if (and (<= x1 px x2) (<= y1 py y2)) true false)))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (if (and
         (contains-point? outer [x1 y1])
         (contains-point? outer [y1 y2]))
      true
      false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (< 1 (author-count book)) true false))

(defn add-author [book new-author]
  (let [new (conj (:authors book) new-author)]
  (assoc book :authors new)))

(defn alive? [author]
  (if (contains? author :death-year) false true))

(defn countIt [x]
  (count x))

(defn element-lengths [collection]
  (map countIt collection))

(defn second-elements [collection]
  (let [munge (fn [x] (get x 1))]
    (map munge collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [setti (set a-seq)]
    (if (= (count setti) (count a-seq)) false true)))

(defn old-book->new-book [book]
  (let [new (set (:authors book))]
    (assoc book :authors new)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [nimi (:name author)
        alkuVuosi (if (contains? author :birth-year) (:birth-year author) "")
        loppuVuosi (if (contains? author :death-year) (:death-year author) "")
        ]
    (str nimi (if (= alkuVuosi "") "" (str " (" alkuVuosi " - " loppuVuosi ")")))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [bookString (cond
                     (= (count books) 0) "No books."
                     (= (count books) 1) "1 book."
                     :else (str (count books) " books."))
   booksString (apply str (interpose ". " (map book->string books)))]
    (if (= bookString "No books.") (str bookString) (str bookString " " booksString "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

;(def authors #{china, felleisen, octavia, friedman})

(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (:name x))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

;(def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
;(def christopher {:name "Christopher Tolkien" :birth-year 1924})
;(def kay {:name "Guy Gavriel Kay" :birth-year 1954})

;(def silmarillion {:title "Silmarillion"
                   ;:authors #{jrrtolkien, christopher, kay}})

;(def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
;(def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})

;(def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
