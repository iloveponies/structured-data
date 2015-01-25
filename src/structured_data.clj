(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

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
  (if (= (width rectangle) (height rectangle))
    true
    false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[x y] point]
    (let [[[x1 y1] [x2 y2]] rectangle]

      (if (and (<= x1 x x2) (<= y1 y y2))
        true
        false
  ))))

(defn contains-rectangle? [outer inner]
  (let[[[inner_x1 inner_y1] [inner_x2 inner_y2]] inner]
        (let [bottom-left (point inner_x1 inner_y1)]
          (let [top-right (point inner_x2 inner_y2)]
             (and (contains-point? outer bottom-left) (contains-point? outer top-right))
  ))))


(defn title-length [book]
  (count (:title book))
  )


(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [auths (conj (:authors book) new-author)]
    (assoc book :authors auths )))

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
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str(repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count (distinct a-seq)) (count a-seq))))


(defn old-book->new-book [book]
  (assoc book :authors (into #{} (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [auth
        (fn [book] (:authors book))]
    (apply clojure.set/union (map auth books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (cond (:death-year author)
    (clojure.string/join [(:name author) " (" (:birth-year author) " - " (:death-year author) ")"])
   (:birth-year author)
        (clojure.string/join [(:name author) " (" (:birth-year author) " - )"])
        :else (:name author))
    )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [stringify (fn[books] (apply str (interpose ". " (map book->string books))))]
  (cond
   (= (count books) 1) (str "1 book. " (stringify books) ".")
    (> (count books) 1) (str (count books) " books. " (stringify books) ".")
   :else "No books."
    )))

(defn books-by-author [author books]
 (filter (fn[book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
