(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))


(defn height [rectangle]
   (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    (and (<= x1 px x2) (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [getFirst (fn [x] (get x 1))]
    (map getFirst collection)))

(defn titles [books]
  (map :title books))

(defn author-names [book]
  (map :name (:authors book)))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors))))

(defn has-author? [book author]
  (contains? (get book :authors) author))



(def china {:name "China Miéville", :birth-year 1972})
(def octavia {:name "Octavia E. Butler"
              :birth-year 1947
              :death-year 2006})
(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})

(def cities {:title "The City and the City" :authors #{china}})
(def wild-seed {:title "Wild Seed", :authors #{octavia}})
(def embassytown {:title "Embassytown", :authors #{china}})
(def little-schemer {:title "The Little Schemer"
                     :authors #{friedman, felleisen}})
(def books [cities, wild-seed, embassytown, little-schemer])

(defn authors [books]
    (set (apply clojure.set/union (map :authors books))))

(defn all-author-names [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply clojure.set/union (map author-names books)))))


(defn author->string [author]
  (str (:name author) (if (:birth-year author)
                        (str " (" (:birth-year author) " - " (:death-year author) ")")
                        )))


(author->string felleisen)

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(authors->string (:authors little-schemer))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(book->string little-schemer)

(defn bah [books]
  (apply str (interpose ". " (map book->string books))))

(defn meh [c]
  (cond
    (== c 0) "No books."
    (== c 1) "1 book. "
    :else (str c " books. ")))

(defn books->string [books]
  (if (empty? books) "No books."
    (str (meh (count books)) (bah books))))

(books->string [])
(books->string [little-schemer, cities, wild-seed])

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
