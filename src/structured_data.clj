(ns structured-data)


(defn do-a-thing [x]
  (let [y (+ x x)]
  (Math/pow y y))
)

(defn spiff [v]
  (if (> (count v) 2) (+ (get v 0) (get v 2)))
  )

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
  (if (> (count v) 2) (+ x z)))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[point1 point2] rectangle
        [x1 y1] point1 [x2 y2] point2]
        (Math/abs (- x1 x2))
  )  )

(defn height [rectangle]
   (let [[point1 point2] rectangle
        [x1 y1] point1 [x2 y2] point2]
        (Math/abs (- y1 y2))
  ))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle) ))

(defn contains-point? [rectangle point]
  (let [[point1 point2] rectangle
        [x1 y1] point1 [x2 y2] point2 [x y] point]
        (and (<= x1 x x2) (<= y1 y y2))
  ))

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner
        [x1 y1] point1 [x2 y2] point2]
        (and (contains-point? outer point1) (contains-point? outer point2))
  ))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors-list (get book :authors)]
  (assoc book :authors (conj authors-list new-author))
  ))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [segundo (fn [x] (get x 1))]
    (map segundo collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors (old-book->new-book book)) author))

(defn authors [books]
  (set (apply concat (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
   (let [authorname (:name author) authorbirth (:birth-year author) authordeath (:death-year author)] (str authorname (if authorbirth (str " (" authorbirth " - " authordeath ")")))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
   (let [bookcount (count books)] (if (zero? bookcount) "No books." (str bookcount " book" (if (> bookcount 1) "s") ". " (apply str (interpose ". " (map book->string books))) "."))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
