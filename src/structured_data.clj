(ns structured-data)

(defn do-a-thing [x]
  (let [dx (+ x x)]
  (Math/pow dx dx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

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
  (let [[[x1 y1][x2 y2]] rectangle]
    do (Math/abs(- x1 x2))))

(defn height [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    do (Math/abs(- y1 y2))))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[xp yp] point]
    (let [[[x1 y1][x2 y2]] rectangle]
      (and (<= x1 xp x2) (<= y1 yp y2)))))

(defn contains-rectangle? [outer inner]
  (let [[in1 in2] inner]
    (and (contains-point? outer in1) (contains-point? outer in2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [added-author (conj (:authors book) new-author)]
  (assoc book :authors added-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [sec (fn [v] (get v 1))]
    (map sec collection)))

(defn titles [books]
  (let [title-of (fn [book] (:title book))]
    (map title-of books)))

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count (set a-seq)) (count a-seq)))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (let [names (fn [book] (map :name (:authors book)))]
    (set (apply concat (map names books)))))

(defn author->string [author]
  (let [author-name (:name author)
        birth (:birth-year author)
        death (:death-year author)]
    (cond
     death (str author-name " (" (str birth) " - " (str death) ")")
     birth (str author-name " (" (str birth) " - )")
     :else (str author-name)
     )))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [n (count books)]
    (cond
     (== n 0) (str "No books.")
     (== n 1) (str n " book. " (book->string (get books 0)) "." )
     :else (str (apply str (interpose ". " (cons (str n " books") (seq (map book->string books))))) "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
