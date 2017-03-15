(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

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
  (- (get (get rectangle 1) 0) (get (get rectangle 0) 0)))

(defn height [rectangle]
  (- (get (get rectangle 1) 1) (get (get rectangle 0) 1)))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[a b] rectangle]
    (and (<= (get a 0) (get point 0) (get b 0))
       (<= (get a 1) (get point 1) (get b 1)))))

(defn contains-rectangle? [outer inner]
  (let [[a b] inner]
    (and (contains-point? outer a)
         (contains-point? outer b))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (get book :authors) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [x] (get x 1))]
    (map second collection)))

(defn titles [books]
    (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
    (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors))))

(defn has-author? [book author]
  (contains? (get book :authors) author))

(defn authors [books]
  (set (apply clojure.set/union (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [[n b d] [(get author :name) (get author :birth-year) (get author :death-year)]]
  (if (= b nil) (str n) (str n " (" b " - " d ")"))))

(defn authors->string [authors]
    (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (get book :title) ", written by " (authors->string (get book :authors))))

(defn books->string [books]
  (let [total (count books)]
    (cond
      (== total 0) (str "No books.")
      (== total 1) (str "1 book. " (book->string (get books 0)) ".")
      (> total 1) (str total " books. " (apply str (interpose ". " (map book->string books))) "."))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (get x :name) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (> (count (living-authors (get book :authors))) 0))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
