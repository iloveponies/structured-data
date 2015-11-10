(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

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
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x3 y3] point]
    (and (<= x1 x3 x2) (<= y1 y3 y2))))

(defn contains-rectangle? [outer inner]
  (and (contains-point? outer (first inner)) (contains-point? outer (second inner))))


(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [x (:authors book)]
    (assoc book :authors (conj x new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [secElem (fn [x] (get x 1))]
    (map secElem collection)))


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
  (< (count (set a-seq)) (count a-seq)))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (set (apply concat (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (apply str (concat [(:name author) (if (contains? author :birth-year)
                                       (apply str (concat [" ("
                                                           (:birth-year author)
                                                           " - "
                                                           (if (not (alive? author))
                                                             (:death-year author))
                                                           ")"])))])))

(defn authors->string [authors]
  (apply str (concat (interpose ", " (map author->string authors)))))

(defn book->string [book]
  (apply str (concat [(:title book) ", written by " (authors->string (:authors book))])))

(defn books->string [books]
  (let [n (count books)]
    (apply str (apply concat [(cond
                         (= n 0) "No books"
                         (= n 1) "1 book. "
                         (> n 1) (apply str (concat [n " books. "])))
                        (interpose ". " (map book->string books))
                        "."]))))


(defn books-by-author [author books]
  (filterv (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors)))

(defn living-authors [authors]
  (filterv alive? authors))

(defn has-a-living-author? [book]
  (> (count (filter alive? (:authors book))) 0))

(defn books-by-living-authors [books]
  (filterv has-a-living-author? books))

; %________%
