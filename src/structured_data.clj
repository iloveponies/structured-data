(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (let [x (get v 0)
        y (get v 2)]
    (+ x y)))

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
     (- x2 x1))
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
     (- y2 y1))
  )

(defn square? [rectangle]
  (if (== (height rectangle) (width rectangle))
    true
    false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let  [[[x1 y1] [x2 y2]] rectangle
          [x3 y3] point]
    (and (<= x1 x3 x2) (<= y1 y3 y2)))
  )

(defn contains-rectangle? [outer inner]
  (let  [[[x1 y1] [x2 y2]] outer
         [[x3 y3] [x4 y4]] inner]
    (and (contains-point? outer [x3 y3]) (contains-point? outer [x4 y4])))
  )

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
  (count (:authors book))
  )

(defn multiple-authors? [book]
  (if (== (author-count book) 1)
    false
    true)
  )

(defn add-author [book new-author]
  (let [all-authors (conj (get book :authors) new-author)]
    (assoc book :authors all-authors))
  )

(defn alive? [author]
  (not (contains?
         author :death-year))
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [second (fn [x] (get x 1))]
    (map second collection))
  )

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq))
  )

(defn stars [n]
  (apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem))
  )

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq)))
  )

(defn old-book->new-book [book]
  (let [authors (set (get book :authors))]
    (assoc book :authors authors))
  )

(defn has-author? [book author]
  (contains? (:authors book) author)
  )

(defn authors [books]
  (apply clojure.set/union (map :authors books))
  )

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (cond (contains? author :death-year) (str (author :name)
                                            " (" (author :birth-year) " - " (author :death-year) ")")
        (contains? author :birth-year) (str (author :name)
                                            " (" (author :birth-year) " - )")
        :else (str (author :name))
        )
  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (str (book :title) ", written by " (authors->string (book :authors)))
  )

(defn books->string [books]
  (let [bookinfo (apply str (interpose ", " (map book->string books)))]
    (cond (== (count books) 0) (str "No books.")
          (== (count books) 1) (str "1 book. " bookinfo ".")
          :else (str (count books) " books. " bookinfo  ".")))
  )

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books)
  )

(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (x :name))) authors))
  )

(defn living-authors [authors]
  (filter alive? authors)
  )

(defn has-a-living-author? [book]
  (< 0 (count (living-authors (book :authors))))
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

; %________%
