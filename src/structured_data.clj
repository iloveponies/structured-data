(ns structured-data)


(defn do-a-thing [x]
  (let [dublex (+ x x)]
   (Math/pow dublex dublex)))


(defn spiff [v]
  (+ (get v 0)  (get v 2)))


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
  (let [ [[x1 _] [x2 _]] rectangle]
  (- x2 x1)) )


(defn height [rectangle]
  (let [ [[_ y1] [_ y2]] rectangle]
  (- y2 y1)) )


(defn square? [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle]
  (= (height rectangle) (width rectangle)) ))


(defn area [rectangle]
  (* (height rectangle) (width rectangle)) )


(defn contains-point? [rectangle point]
  (let [ [[x1 y1] [x2 y2]] rectangle
         [px py]           point]
    (and (<= x1 px x2) (<= y1 py y2))))


(defn contains-rectangle? [outer inner]
  (let [ [p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2) )))


(defn title-length [book]
  (count (book :title)))


(defn author-count [book]
  (count (book :authors)))


(defn multiple-authors? [book]
  (< 1 (count (book :authors))))


(defn add-author [book new-author]
  (assoc book :authors (conj (book :authors) new-author)))


(defn alive? [author]
  (not (contains? author :death-year)))


(defn element-lengths [collection]
  (map count collection))


(defn second-elements [collection]
  (let [scnd (fn[x] (get x 1))]
  (map scnd collection)))


(defn titles [books]
  (map :title books))


(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))


(defn stars [n]
  (apply str (repeat n "*")))


(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))


(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))


(defn old-book->new-book [book]
  (assoc book :authors (set (book :authors))))


(defn has-author? [book author]
  (contains? (book :authors) author))


(defn authors [books]
  (apply clojure.set/union (map :authors books)))


(defn all-author-names [books]
  (set (map :name (authors books))))


(defn author->string [author]
  (str (:name author)
       (if (:birth-year author)
         (str " ("
              (:birth-year author)
              " - "
              (:death-year author)
              ")"))))


(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors) )))


(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))


(defn books->string [books]
  (cond
    (= 0 (count books)) "No books."
    :else (apply str (count books)
                     (if (= 1 (count books)) " book. " " books. ")
                     (apply str (interpose ". " (map book->string books)))
                     ".")))


(defn books-by-author [author books]
  (filter #(contains? (%1 :authors) author) books))


(defn author-by-name [name authors]
  (first (filter #(= (%1 :name) name) authors)))


(defn living-authors [authors]
  (filter #(alive? %1) authors))


(defn has-a-living-author? [book]
  (not (empty? (living-authors (book :authors)))))


(defn books-by-living-authors [books]
  (filter #(has-a-living-author? %1) books))


; %________%
