(ns structured-data)

(defn do-a-thing [x]
  (let [doubled-x (+ x x)]
    (Math/pow doubled-x doubled-x)))


(defn spiff
  "takes a vector and returns the sum of the first and third elements "
  [v]
  (+ (get v 0)
     (get v 2)))


(defn cutify
  "takes a vector as a parameter and adds '<3'"
  [v]
  (conj v "<3"))

(defn spiff-destructuring
  "takes a vector and returns the sum of the first and third elements "
  [v]
  (let [[v1 v2 v3] v]
    (+ v1 v3)))

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
  (let [[[x1 y1] [x2 y2]] rectangle]
    (= (height rectangle) (width rectangle))))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (height rectangle) (width rectangle))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    (and (<= x1 px x2)
         (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1)
         (contains-point? outer p2))))


(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1)
  )

(defn add-author [book new-author]
  (let [authors (:authors book)
        ]
    (assoc book :authors
           (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (map (fn [x] (get x 1)) collection))

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle
  "removes elem from a-set if a-set contains elem, and adds it to the set otherwise."
  [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq))))
  )

(defn old-book->new-book [book]
  (let [authors (set (:authors book))]
    (assoc book :authors authors)
    ))


(defn has-author? [book author]
  (contains? (:authors book) author)
  )

(defn authors
  "returns the authors of every book in books as a set."
  [books]
  (apply clojure.set/union (map :authors books)))

;; GJG 
(defn author-names [book]
  (map :name (:authors book)))

(defn all-author-names [books]
  (set (map :name (authors books))))



(defn author->string [author]
  (let [name (:name author)
        byear (:birth-year author)
        dyear (:death-year author)]
    (cond dyear (format "%s (%s - %s)", name, byear, dyear)
          byear (format "%s (%s - )" name, byear)
          :else name
          )))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))


(defn book->string [book]
  (format "%s, written by %s" (:title book) (authors->string (:authors book))))

(defn books->string [books]
  (let [num-books (count books)
        ]
    (cond (= num-books 0) "No books."
          (= num-books 1) (format "1 book. %s." (book->string (first books)))
          :else (format "%d books. %s." num-books
                        (apply str (interpose ", " (map book->string books))))
          )))

(defn books-by-author [author books]
  (filter #(not (nil? %)) (map (fn [b] (when (has-author? b author) b)) books)))

(defn author-by-name [name authors]
  (first (filter #(not (nil? %)) (map (fn [x] (when (= name (:name x)) x)) authors))))


(defn living-authors [authors]
  (filter #(not (nil? %)) (map (fn [x] (when (alive? x) x)) authors)))


(defn has-a-living-author? [book]
  (first (or (map #(alive? %) (:authors book)))))


(defn books-by-living-authors [books]
  ;; (filter #(not (nil? %)) (map (fn [book] (when (has-a-living-author? book) book)) books)))
  (filter (fn [book] (has-a-living-author? book)) books))

                                        ; %________%
