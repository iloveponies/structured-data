(ns structured-data)


(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))


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
  (let [[[x1] [x2]] rectangle]
      (Math/abs(- x1 x2))))


(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
      (Math/abs(- y2 y1))))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle
        h (Math/abs(- y2 y1))
        w (Math/abs(- x1 x2))]
    (== h w)))


(defn area [rectangle]
  (* (width rectangle) (height rectangle)))


(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    (if (and (>= px x1)
             (<= px x2)
             (>= py y1)
             (<= py y2))
      true false)))


(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
    (and (contains-point? outer bottom-left)
         (contains-point? outer top-right))))


(defn title-length [book]
  (count (:title book)))


(defn author-count [book]
  (count (:authors book)))


(defn multiple-authors? [book]
  (> (author-count book) 1))


(defn add-author [book new-author]
  (let [authors (conj (:authors book) new-author)]
    (assoc book :authors authors)))


(defn alive? [author]
  (not (contains? author :death-year)))


(defn element-lengths [collection]
  (map count collection))


(defn second-elements [collection]
  (let [seconds (fn [v] (get v 1))]
    (map seconds collection)))


(defn titles [books]
  (map (fn [book] (:title book)) books))


(defn monotonic? [a-seq]
  (or
   (apply <= a-seq)
   (apply >= a-seq)))


(defn stars [n]
  (apply str (repeat n "*")))


(defn toggle [a-set elem]
  (if
    (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))


(defn contains-duplicates? [a-seq]
   (not=
    (count a-seq)
     (count (set a-seq))))


(defn old-book->new-book [book]
  (let [authors (set (:authors book))]
    (assoc book :authors authors)))


(defn has-author? [book author]
  (contains? (:authors book) author))


(defn authors [books]
  (let [book-authors (fn [book] (:authors book))]
    (apply clojure.set/union (map book-authors books))))


(defn all-author-names [books]
  (set (map :name (authors books))))


(defn author->string [author]
  (let [name (:name author)
        birth (:birth-year author)
        death (:death-year author)]
    (if (not (nil? death))
      (str name " (" birth " - " death ")")
        (if (not (nil? birth))
          (str name " (" birth " - )")
          (str name)))))


(defn authors->string [authors]
  (let [single-author
        (fn [author] (author->string author))]
    (apply str (interpose ", " (map single-author authors)))))

(defn book->string [book]
  (str
   (:title book)
   ", written by "
   (authors->string (:authors book))))


(defn books->string [books]
  (cond
   (= (count books) 0) "No books."
   (= (count books) 1)
     (apply str "1 book. "
            (book->string (first books))
            ".")
   :else
     (let [single-book
           (fn [book] (book->string book))
           amount (str (count books) " books. ")]
       (apply str amount (apply str (interpose ". " (map single-book books))) "." ))))


(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))


(defn author-by-name [name authors]
  (let [a
        (filter (fn [author] (= name (:name author))) authors)]
    (if (= 0 (count a)) nil
      (first a))))


(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))


(defn has-a-living-author? [book]
  (let [authors (:authors book)]
    (not
     (empty?
      (filter (fn [author] (alive? author)) authors)))))


(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

