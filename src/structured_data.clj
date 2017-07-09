(ns structured-data)

(defn do-a-thing [x]
  (let [twoX (+ x x)]
    (Math/pow twoX twoX)))

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
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (let [h (height rectangle)
        w (width rectangle)]
    (== w h)))

(defn area [rectangle]
  (let [h (height rectangle)
        w (width rectangle)]
    (* w h)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and (contains-point? outer (point x1 y1))
         (contains-point? outer (point x2 y2)))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [result (conj (:authors book) new-author)]
    (assoc book :authors result)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [seconds (fn [x] (get x 1))]
    (map seconds collection)))

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
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [new-authors (set (:authors book))]
    (assoc book :authors new-authors)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
   (set (map :name (authors books))))

(defn author->string [author]
  (let [result (:name author)]
    (cond
      (contains? author :death-year) (str result " (" (:birth-year author) " - " (:death-year author) ")")
      (contains? author :birth-year) (str result " (" (:birth-year author) " - )")
      :else result)))

(defn authors->string [authors]
  (let [result (map author->string authors)]
    (apply str (interpose ", " result))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
    (cond
    (== (count books) 0) "No books."
    :else (let [result (count books)]
            (cond
              (== 1 result) (str result " book. " (apply str (map book->string books)) ".")
              :else (str result " books. " (apply str (interpose ". " (map book->string books))) ".")))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
    (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
    (filter (fn [book] (has-a-living-author? book)) books))


; %________%
