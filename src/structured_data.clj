(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x) ]
    (Math/pow xx xx)))

(defn spiff [v]
  (if (< (count v) 3)
    nil
    (+ (get v 0) (get v 2))))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (if (< (count v) 3)
    nil
    (let [[a b c] v]
      (+ a c))))

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
  (let [height (height rectangle)
        width (width rectangle)]
    (if (= height width) true false)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[r-x1 r-y1] [r-x2 r-y2]] rectangle
        [p-x p-y] point]
    (and (and (>= p-x r-x1 ) (<= p-x r-x2)) (and (>= p-y r-y1 ) (<= p-y r-y2)))))


(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
    (and (contains-point? outer bottom-left) (contains-point? outer top-right))))

(defn title-length [book]
  (count(:title book)))

(defn author-count [book]
  (count(:authors book)))

(defn multiple-authors? [book]
  (if (<= (author-count book) 1) false true))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-element (fn [x] (get x 1))]
    (map second-element collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (cond
    (= (count a-seq) 1) false
    (contains? (set(rest a-seq)) (first a-seq)) true
    :else (contains-duplicates? (rest a-seq))
  )
)

(defn old-book->new-book [book]
  (assoc book :authors (set(:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set(map :name (authors books))))

(defn author->string [author]
  (let [a-name (:name author)
        a-birth-year (:birth-year author)
        a-death-year (:death-year author)]
    (if a-birth-year
      (str a-name " (" a-birth-year " - " a-death-year ")")
      (str a-name))))

(defn authors->string [authors]
  (apply str (interpose ", " (set(map author->string authors)))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [number_of_books (count books)]
    (cond
      (= number_of_books 0) "No books."
      (= number_of_books 1) (str number_of_books " book. " (apply str(map book->string books)) ".")
      :else (str number_of_books " books. " (apply str (interpose ". " (seq(map book->string books)))) ".")
    )))

(defn books-by-author [author books]
  (filter (fn [book] (contains? (:authors book) author)) books))

(defn author-by-name [name authors]
  (if (= (count authors) 0)
    nil
    (if (= (:name (first authors)) name)
      (first authors)
      (author-by-name name (rest authors))
    )
  ))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
