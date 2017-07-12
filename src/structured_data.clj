(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v] (let [f (get v 0 0)
                      t (get v 2 0)]
                  (+ f t)))

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
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    (and (<= x1 px x2)
         (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[bl tr] inner]
    (and
      (contains-point? outer bl)
      (contains-point? outer tr))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (let [current-year 2017
        death-year (get author :death-year)]
    (or (nil? death-year) (< current-year death-year))))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [scnd (fn [c] (get c 1))]
    (map scnd collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [as (set a-seq)]
    (not (= (count a-seq) (count as)))))

(defn old-book->new-book [book]
  (let [av (:authors book)]
    (assoc book :authors (set av))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (set (apply clojure.set/union (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [n (:name author)
        by (:birth-year author)
        dy (:death-year author "")
        ys (if (nil? by) "" (str " (" by " - " dy ")"))] (str n ys)))


(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let
    [bs (if (> (count books) 1) " books. " " book. ")]
    (if (empty? books)
      "No books."
      (str (count books) bs (apply str (interpose ". " (map book->string books))) "."))))

(defn books-by-author [author books]
  (let [ha (fn [b] (has-author? b author))]
    (filter ha books)))

(defn author-by-name [name authors]
  (first (filter (fn [b] (= (:name b) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
