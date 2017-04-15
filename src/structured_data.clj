(ns structured-data)

(defn do-a-thing [x]
  (let [summa (+ x x)]
    (Math/pow summa summa)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a _ b] v]
    (+ a b))) 

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
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and
      (<= x1 x x2)
      (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (and
    (contains-point? outer (get inner 0))
    (contains-point? outer (get inner 1))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (count (:authors book))))

(defn add-author [book new-author]
  (let [authors (conj (:authors book) new-author)]
    (assoc book :authors authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [seconds (fn [col] (get col 1))]
    (map seconds collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (cond
    (contains? a-set elem) (disj a-set elem)
    :else (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (< (count (set a-seq)) (count a-seq)))

(defn old-book->new-book [book]
  (let [authors (:authors book)]
    (assoc book :authors (set authors))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [namae (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)
        sb (new StringBuilder)]
    (.append sb (str namae))
    (if birth-year 
      (.append sb (str " (" birth-year " - ")))
    (if death-year
      (.append sb (str death-year)))
    (if birth-year
      (.append sb (str ")")))
    (.toString sb)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        authors (authors->string (:authors book))]
    (str title ", written by " authors)))

(defn count-pluralize [col word]
  (let [len (count col)]
    (cond
      (== 1 (count col)) (str len " " word)
      :else (str len " " word "s"))))

(defn books->string [books]
  (let [countbooks (count-pluralize books "book")
        bstring (map book->string books)]
    (cond
      (> 1 (count books)) (str "No books.")
      :else (str countbooks ". "
                 (apply str (interpose ", " (map book->string books)))
                 "." ))))

(defn books-by-author [author books]
  (let [check-author (fn [book] (has-author? book author))]
    (filter check-author books)))

(defn author-by-name [namae authors]
  (let [check-name (fn [author] (= (:name author) namae))]
    (first (filter check-name authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

