(ns structured-data)

(defn do-a-thing [x]
  (let [two_x (+ x x)]
    (Math/pow two_x two_x)
)
)

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x1 x2 x3] v]
    (+ x1 x3)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
    )
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
    )
  )

(defn square? [rectangle]
(= (height rectangle) (width rectangle))
  )

(defn area [rectangle]
(* (height rectangle) (width rectangle))
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point]
    (and (<= x1 xp x2) (<= y1 yp y2))
    )
  )

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
        [[ix1 iy1] [ix2 iy2]] inner]
    (and (<= x1 ix1 ix2 x2) (<= y1 iy1 iy2 y2))
    )
  )

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
  (count (:authors book))
  )

(defn multiple-authors? [book]
  (> (author-count book) 1)
  )

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author))
  )

(defn alive? [author]
  (if (:death-year author) false true)
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [pull (fn [x] (get x 1))]
    (map pull collection))
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
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
  )

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (apply conj #{} a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))

  )

(defn has-author? [book author]
  (if (contains? (:authors book) author) true false)
  )

(defn authors [books]
  (apply clojure.set/union (map :authors books))
  )

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (let [name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)]
    (str name (if birth-year (str " (" birth-year " - " death-year ")") ""))
    )
)

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book)))
  )

(defn books->string [books]
                      ( let [cnt (count books)]
                        (cond
                         (= 0 cnt) (str "No books.")
                         (= 1 cnt) (str "1 book. " (book->string (first books)) ".")
                         :else (str cnt " books. " (apply str (interpose ". " (map book->string books))))
                         )))
  

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)
)

(defn author-by-name [name authors]
  (let [has-author? (fn [author] (= (:name author) name))]
    (first (filter has-author? authors))
    )
)

(defn living-authors [authors]
  (filter alive? authors)
  )

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book))))
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

; %________%
