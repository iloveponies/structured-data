(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(do-a-thing 2)

(defn spiff [v]
  (+
    (get v 0)
    (get v 2)))

;=============================================

(defn cutify [v]
  (conj v "<3"))

;=============================================

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

;=============================================

(defn point [x y]
  [x y])

;=============================================

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

;=============================================

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

;=============================================

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

;=============================================

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

;=============================================

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

;=============================================

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[px py] point]
      (and
        (<= x1 px x2)
        (<= y1 py y2)))))

;=============================================

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and
      (contains-point? outer [x1 y1])
      (contains-point? outer [x2 y2]))))

;=============================================


(defn title-length [book]
  (count (:title book)))

;=============================================

(defn author-count [book]
  (count (:authors book)))

;=============================================

(defn multiple-authors? [book]
  (< 1 (author-count book)))

;=============================================

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

;=============================================

(defn alive? [author]
  (not (contains? author :death-year)))

;=============================================

(defn element-lengths [collection]
  (map count collection))

;=============================================

(defn second-elements [collection]
  (let [second-item (fn [subcoll] (get subcoll 1))]
    (map second-item collection)))

;=============================================

(defn titles [books]
  (map :title books))

;=============================================

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)))

;=============================================

(defn stars [n]
  (apply str (repeat n "*")))

;=============================================

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

;=============================================

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq))))

;=============================================

(defn old-book->new-book [book]
  (let [authors-coll (:authors book)]
    (assoc book :authors (set authors-coll))))

;=============================================

(defn has-author? [book author]
  (contains? (:authors book) author))

;=============================================

(defn authors [books]
  (let [authors-set
        (fn [book] (set (:authors book)))]
    (apply clojure.set/union (map authors-set books))))

;=============================================

(defn all-author-names [books]
  (set (map :name (authors books))))

;=============================================

(defn author->string [author]
  (let [na (:name author)
        by (:birth-year author)
        dy (:death-year author)]
        (if (or by dy)
          (str na " (" by " - " dy ")")
          na)))

;=============================================

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

;=============================================

(defn book->string [book]
  (str
    (:title book)
    ", written by "
    (authors->string (:authors book))))

;=============================================

(defn books->string [books]
  (if (empty? books)
    (str "No books.")
    (str
      (count books) " book"
      (if (< 1 (count books)) "s")
      ". "
      (apply str (interpose ", " (map book->string books)))
      ".")))

;=============================================

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

;=============================================

(defn author-by-name [name authors]
  (first
    (filter (fn [author] (= name (:name author))) authors)))

;=============================================

(defn living-authors [authors]
  (set
    (filter (fn [author] (alive? author)) authors)))

;=============================================

(defn has-a-living-author? [book]
  (let [authors (:authors book)]
    (not (empty? (set
                   (filter (living-authors authors) authors))))))

;=============================================

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))


; %________%
