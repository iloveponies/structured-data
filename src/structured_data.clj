(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

(defn spiff [v]
  (if (> (count v) 2)
    (+ (get v 0) (get v 2))
    nil))


(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (if (> (count v) 2)
    (let [[a b c] v]
      (+ a c))
    nil))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[blx bly] [trx try]] rectangle]
    (- trx blx)))

(defn height [rectangle]
  (let [[[blx bly] [trx try]] rectangle]
    (- try bly)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[blx bly] [trx try]] rectangle
        [x y] point]
    (and (<= blx x trx) (<= bly y try))))

(defn contains-rectangle? [outer inner]
  (let [[bl tr] inner]
    (and (contains-point? outer bl) (contains-point? outer tr))))

(def china {:name "China Miéville", :birth-year 1972})
(def octavia {:name       "Octavia E. Butler"
              :birth-year 1947
              :death-year 2006})
(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})

(def cities {:title "The City and the City" :authors #{china}})
(def wild-seed {:title "Wild Seed", :authors #{octavia}})
(def embassytown {:title "Embassytown", :authors #{china}})
(def little-schemer {:title   "The Little Schemer"
                     :authors #{friedman, felleisen}})

(assoc cities :awards ["Hugo", "World Fantasy Award",
                       "Arthur C. Clarke Award",
                       "British Science Fiction Award"])

(def all-books [cities, wild-seed, embassytown, little-schemer])

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [new-authors (conj (:authors book) new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-element (fn [x] (first (rest x)))]
    (map second-element collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [a-set (set a-seq)]
    (not (= (count a-set) (count a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn author-names [book]
  (map :name (:authors book)))

(defn authors [books]
  (clojure.set/union (set (apply concat (map :authors books)))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        by (:birth-year author)
        dy (:death-year author)
        dates (if dy
                (str " (" by " - " dy ")")
                (if by
                  (str " (" by " - )")
                  ""))]
    (str name dates)
    ))

(defn authors->string [authors]
  (apply str( interpose ", " (map author->string authors))) )

(defn book->string [book]
  (str (:title book) (str ", written by ") (authors->string (:authors book))))

(defn books->string [books]
  (let [book-count (count books)]
    (if (= book-count 0)
      (str "No books.")
      (str (if (= book-count 1)
        (str book-count " book. ")
        (str book-count " books. ")) (apply str (interpose ". " (map book->string books))) "."))))

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