(ns structured-data)

(defn do-a-thing [x]
  (let [dub_x (+ x x)]
    (Math/pow dub_x dub_x)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[first _ second] v]
    (+ first second)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 _] [x2 _]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[_ y1] [_ y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    (and (<= x1 px x2) (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[bottom-left upper-right] inner]
    (and (contains-point? outer bottom-left) (contains-point? outer upper-right))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)
        new-authors (conj authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [my-second (fn [v] (get v 1))]
    (map my-second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply >= a-seq)
      (apply <= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count (set a-seq)) (count a-seq))))

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
  (let [death (or (:death-year author) "")
        birth (:birth-year author)
        life (if birth (format " (%s - %s)" birth death)
               "")]
    (format "%s%s" (:name author) life)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (format "%s, written by %s"
          (:title book) (authors->string (:authors book))))

(defn books->string [books]
  (if (not (empty? books))
    (let [book-str (format "%s." (apply str (interpose ", " (map book->string books))))
          book-count-str (if (> (count books) 1)
                           (str (count books) " books. ")
                           "1 book. ")]
      (str book-count-str book-str))
    "No books."))

(defn books-by-author [author books]
  (filter (fn [bk] (has-author? bk author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [a] (= (:name a) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
