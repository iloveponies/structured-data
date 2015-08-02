(ns structured-data)

(defn abs [x] (max x (- x)))

(defn do-a-thing [x]
  (let [sum (* 2 x)] (Math/pow sum sum)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v] (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (abs (- x2 x1)))

(defn height [[[x1 y1] [x2 y2]]]
  (abs (- y2 y1)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [x3 y3]]
  (and (<= x1 x3 x2) (<= y1 y3 y2)))

(defn contains-rectangle? [r [w z]]
  (and (contains-point? r w) (contains-point? r z)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (not (= 1 (author-count book))))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map (fn [x] (get x 1)) collection))

(defn titles [books]
  (map (fn [x] (:title x)) books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (book :authors) author))

(defn authors [books]
  (apply clojure.set/union (map (fn [x] (:authors x)) books)))

(defn all-author-names [books]
  (set (map (fn [x] (:name x)) (authors books))))

(defn author->string [author]
  (str
   (:name author)
   (if (contains? author :birth-year)
     (str " (" (:birth-year author) " - "
          (if (contains? author :death-year) (:death-year author))
          ")"
     )
   )
  )
)

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [bc (count books)]
    (if (= 0 bc) "No books."
      (str bc " book" (if (not (= 1 bc)) "s") ". "
           (apply str (interpose ". " (map book->string books)))
           "." )
      )
    )
  )

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (filter (fn [x] (= name (:name x))) authors)
  )

(defn living-authors [authors]
  (filter alive? authors)
  )

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

; %________%
