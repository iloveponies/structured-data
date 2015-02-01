(ns structured-data)

(defn do-a-thing [x]
  (let [doubl (+ x x)]
    (Math/pow doubl doubl)
    ))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)
    ))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (<= x1 x2)
      (- x2 x1)
      (- x1 x2)
    )
  ))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (<= y1 y2)
      (- y2 y1)
      (- y1 y2)
    )
  ))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle))
  )

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [xp yp] point]
    (and (<= x1 xp x2) (<= y1 yp y2))
    )
  )

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer [[xx1 yy1] [xx2 yy2]] inner]
    (and (<= x1 xx1 xx2 x2 ) (<= y1 yy1 yy2 y2))
    )
  )

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
 (assoc book :authors (conj (:authors book) new-author))
  )

(defn alive? [author]
  (= nil (:death-year author)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [getsec (fn [cl] (get cl 1))]
    (map getsec collection)
    )
  )

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    ))

(defn contains-duplicates? [a-seq]
  (not= (count(set a-seq)) (count a-seq)))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author)
  )

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (str (:name author)
     (if (:birth-year author)
       (str " (" (:birth-year author) " - " (:death-year author) ")")
       nil
       )))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (case (count books)
    0 "No books."
    1 (str "1 book. " (apply str (interpose ". " (map book->string books))) ".")
    (str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")
    ))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter #(= name (:name %)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [x (living-authors (:authors book))]
    (not (empty? x))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
