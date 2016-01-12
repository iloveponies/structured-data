(ns structured-data)

(defn do-a-thing [x]
  (let [plussaa (+ x x)]
    (Math/pow plussaa plussaa)))




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
  (if (== (height rectangle) (width rectangle)) true false))




(defn area [rectangle]
  (* (height rectangle) (width rectangle)))



(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [a b] point]
    (if (and (<= x1 a x2) (<= y1 b y2)) true false)))





(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (if (and (contains-point? outer (point x1 y1)) (contains-point? outer (point x2 y2))) true false)))





(defn title-length [book]
  (count (:title book)))




(defn author-count [book]
  (count (:authors book)))



(defn multiple-authors? [book]
  (if (< 1 (author-count book)) true false))




(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))





(defn alive? [author]
  (if (contains? author :death-year) false true))




(defn element-lengths [collection]
  (map count collection))





(defn second-elements [collection]
  (let [get-second (fn [coll] (get coll 1))]
    (map get-second collection)))






(defn titles [books]
  (let [get-bookname (fn [book] (:title book))]
    (map get-bookname books)))





(defn monotonic? [a-seq]
  (let [is-greater (fn [sequence] (apply <= sequence))
        is-smaller (fn [seq] (apply >= seq))]
    (if (or (is-smaller a-seq) (is-greater a-seq)) true false)))




(defn stars [n]
  (apply str (repeat n "*")))



(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))




(defn contains-duplicates? [a-seq]
  (let [setify (fn [sequence] (set sequence))
        set-length (fn [sequence] (count (setify sequence)))]
    (if (== (set-length a-seq) (count a-seq)) false true)))




(defn old-book->new-book [book]
  (let [setify (fn [sequence] (set sequence))]
    (assoc book :authors (setify (:authors book)))))



(defn has-author? [book author]
  (if (contains? (:authors book) author) true false))




(defn authors [books]
  (apply clojure.set/union (map :authors books)))



(defn all-author-names [books]
  (set (map :name (authors books))))



(defn author->string [author]
  (let [getname (fn [auth] (:name auth))
        getyear (fn [a] (if (:birth-year a) (str " (" (:birth-year a) " - " (:death-year a) ")") ""))]
      (str (getname author) (getyear author))))



(defn authors->string [authors]
  (let [all-author-strings (fn [all-a] (map author->string all-a))]
    (apply str (interpose ", " (all-author-strings authors)))))



(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))



(defn books->string [books]
  (let [getbookcount (fn [all-books] (if (> (count all-books) 1) (str (count all-books) " books. ") "1 book. "))
        getallbooks (fn [abooks] (apply str (interpose ". " (map book->string abooks))))]
  (if (> (count books) 0) (str (getbookcount books) (getallbooks books) ".") "No books.")))



(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))



(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))



(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))



(defn has-a-living-author? [book]
  (if (empty? (living-authors (:authors book))) false true))


(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))



; %________%
