(ns structured-data)

(defn do-a-thing [x]
  (let [sum (+ x x)]
    (Math/pow sum sum))
  )

(defn spiff [v]
  (+ (get v 0) (get v 2))
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[a x b] v] (+ a b))
  )

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
 (let [[[x1 y1] [x2 y2]] rectangle]
  (= (- x2 x1) (- y2 y1))
  ))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (* (- x2 x1) (- y2 y1))
  ))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
      [px py] point]
    (and (<= x1 px x2) (<= y1 py y2)))
  )

(defn contains-rectangle? [outer inner]
  (let [[[ox1 oy1] [ox2 oy2]] outer
        [p1 p2] inner]
   (and (contains-point? outer p1) (contains-point? outer p2)))
  )

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
  (count (:authors book))
  )

(defn multiple-authors? [book]
  (< 1 (count (:authors book)))
  )

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author))
  )

(defn alive? [author]
  (not (contains? author :death-year))
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [get-second (fn [x] (get x 1))]
    (map get-second collection)
  ))

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
 (or  (apply <= a-seq) (apply >= a-seq))
  )

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
  )

(defn contains-duplicates? [a-seq]
 (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))
  

(defn has-author? [book author]
  (contains? (:authors book) author)
 )

(defn authors [books]
  (let [single-author (fn [book] (:authors book))]
     (apply clojure.set/union (map single-author books))
  ))

  (defn all-author-names [books]
      (let [author-names
                     (fn [book] (map :name (:authors book)))]
            (set (apply concat (map author-names books)))))

(defn author->string [author]
  (let [name (str (:name author))
        years (if (:birth-year author) (str "("(:birth-year author) " - " (:death-year author)")"))]
    (str name (if years (str " " years)))
  ))

(defn authors->string [authors]
 (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))) 
  )

(defn books->string [books]
  (let [nbooks (count books)]
    (cond
      (= nbooks 1) (str nbooks " book. " (apply str (interpose ". " (map book->string books))) ".")
      (> nbooks 1) (str nbooks " books. " (apply str (interpose ". " (map book->string books))) ".")
      :else (str "No books."))
  ))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books)
  )

(defn author-by-name [name authors]
 (first (filter (fn [x] (= (:name x) name)) authors)))

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors)
  )

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))
  )

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books)
  )

; %________%
