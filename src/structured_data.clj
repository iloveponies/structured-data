(ns structured-data)

(defn do-a-thing [x]
  (let [double_x (+ x x)]
    (java.lang.Math/pow double_x double_x)))

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
    (java.lang.Math/abs(- x2 x1))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (java.lang.Math/abs(- y2 y1))))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (== (width rectangle) (height rectangle)) true false)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (if (and (>= y2 y y1) (>= x2 x x1)) true false)))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (if (and (contains-point? outer [x1 y1]) 
             (contains-point? outer [x2 y2])) true false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (let [original-authors (:authors book)
        new-authors (conj original-authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (if (contains? author :death-year) false true))

(defn element-lengths [collection]
  (map count (seq collection)))

(defn second-elements [collection]
  (let [second-element (fn [coll] (get coll 1))]
    (map second-element (seq collection))))

(defn titles [books]
  (map :title (seq books)))

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq)) true false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (cond
    (contains? a-set elem) (disj a-set elem)
    :else (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [set-list (set a-seq)]
    (if (== (count a-seq) (count set-list)) false true)))

(defn old-book->new-book [book]
  (let [authors-as-set (fn [book] (set (:authors book)))]
    (assoc book :authors (authors-as-set book))))

(defn has-author? [book author]
  (let [author-names (fn [book] (set (map :name (:authors book))))]
    (if (contains? (author-names book) (:name author)) true false)))

(defn authors [books]
  (let [all-authors (map :authors books)]
    (apply clojure.set/union all-authors)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birth-year (fn [author] (if (contains? author :birth-year) (str " (" (str (:birth-year author)) " -") ""))
        death-year (fn [author] (if (contains? author :death-year) (str " " (str (:death-year author)) ")") (if (contains? author :birth-year) " )" "")))]
    (str name (birth-year author) (death-year author))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [books->text (fn [books] (apply str (interpose ". " (map book->string books))))
        book-count-text (fn [books] (cond 
                                      (== (count books) 0) "No books"
                                      (== (count books) 1) "1 book. "
                                      :else (str (count books) " books. ")))]
    (str (book-count-text books) (books->text books) ".")))

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

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))
    
(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (if (empty? (filter (fn [author] (alive? author)) (:authors book))) false true))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
