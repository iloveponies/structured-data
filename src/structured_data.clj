(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (if (= (get v 2) nil)
    "?" (+ (get v 0) (get v 2))))


(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (if (= c nil) "?" (+ a c))))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 x2] [y1 y2]] rectangle]
    (- y1 x1)))


(defn height [rectangle]
  (let [[[x1 x2] [y1 y2]] rectangle]
    (- y2 x2)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (= (width [[x1 y1] [x2 y2]]) (height [[x1 y1] [x2 y2]])) true false)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
    [z1 z2] point]
      (if (and (<= x1 z1 x2)  (<= y1 z2 y2)) true false)))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
    [[a1 b1] [a2 b2]] inner]
      (if (and (<= x1 a1) (<= y1 b1) (>= x2 a2) (>= y2 b2)) true false)))

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
;(def books [cities, wild-seed, embassytown, little-schemer, silmarillion, deus-irae])

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (if (contains? author :death-year) false true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second_el (fn [x] (get x 1))]
    (map second_el collection)))

(defn titles [books]
  (let [fn_title (fn [x] (:title x))]
    (map fn_title books)))

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq)) true false))

(defn stars [n]
  (apply str ((fn [x] (repeat x "*")) n)))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (== (count (set a-seq)) (count a-seq)) false true))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

;(defn all-author-names [books]
;  (let [author-names
;         (fn [book] (map :name (:authors book)))]
;    (set (apply concat (map author-names books)))))

(defn all-author-names [books]
  (set (map :name (authors books))))


(defn author->string [author]
  (let [name (:name author)
      by (:birth-year author)
      dy (:death-year author)]
  (cond
    by (str name " (" by " - " dy ")")
   :else (str name))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (apply str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (cond
   (== (count books) 0) "No books."
   (== (count books) 1) (str (apply str (count books) " book. " (interpose ". " (map book->string books))) ".")
   (> (count books) 1) (str (apply str (count books) " books. " (interpose ". " (map book->string books))) ".")))

(defn books-by-author [author books]
  (filter (fn [b] (has-author? b author)) books))

;(def authors #{china, felleisen, octavia, friedman, jrrtolkien, christopher, kay, dick, zelazny})

(defn author-by-name [name authors]
  (first (filter (fn [a] (= (:name a) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))


;(def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
;(def christopher {:name "Christopher Tolkien" :birth-year 1924})
;(def kay {:name "Guy Gavriel Kay" :birth-year 1954})

;(def silmarillion {:title "Silmarillion"
;                   :authors #{jrrtolkien, christopher, kay}})

;(def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
;(def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})

;(def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})

(defn has-a-living-author? [book]
  (if (empty? (filter alive? (:authors book))) false true))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
