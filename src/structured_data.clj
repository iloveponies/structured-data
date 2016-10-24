(ns structured-data)

(defn do-a-thing [x]
  (let [dub (+ x x)]
    (Math/pow dub dub)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))
(spiff [1 2 3])
(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

(defn point [x y]
  [x y])
(defn abs [n] (max n (- n)))
(defn rectangle [bottom-left top-right]
  [bottom-left top-right])
(rectangle [1 1] [5 1])
(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1))
(width (rectangle [1 1] [5 1]))
(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(defn square? [[[x1 y1] [x2 y2]]]
  (== (abs (- x2 x1)) (abs (- y2 y1))))
(square? (rectangle [3 2] [4 0]))
(defn area [[[x1 y1] [x2 y2]]]
  (* (abs (- x2 x1)) (abs (- y2 y1))))

(defn contains-point? [[[x1 y1] [x2 y2]] [a b]]
  (and (or (<= x1 a x2) (>= x1 a x2))
       (or (<= y1 b y2) (>= y1 b y2))
       ))

(defn contains-rectangle? [[[x1 y1] [x2 y2]] [[a b] [c d]]]
  (and (contains-point? [[x1 y1] [x2 y2]] [a b])
       (contains-point? [[x1 y1] [x2 y2]] [c d])
       ))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [extras (conj (:authors book) new-author)]
    (assoc book :authors extras)
      ))

(defn alive? [author]
  (not (:death-year author)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [sec (fn [vec] (get vec 1))]
    (map sec collection)
    )
  )

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)
    )
  )
(monotonic? [3 2 1])
(defn stars [n]
  (apply str (repeat n "*")))
(stars 3)
(apply str (repeat 4 "*"))
(defn toggle [a-set elem]
  (if
    (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )
  )

(defn contains-duplicates? [a-seq]
  (< (count (set a-seq)) (count a-seq)))

(defn old-book->new-book [book]
  (let [aset (set (:authors book))]
    (assoc book :authors aset)
    )
  )

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [aname (:name author)
        DOB (:birth-year author)
        RIP (:death-year author)]
    (if DOB
      (str aname " (" DOB " - " RIP ")")
      (str aname)
      )))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (apply str (interpose ", written by " [(:title book) (authors->string (:authors book))])))

(defn books->string [books]
  (cond
    (= (count books) 1) (str "1 book. " (apply str (map book->string books)) ".")
    (> (count books) 1) (str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")
    :else               "No books."
    ))

(defn books-by-author [author books]
  (filter (fn [book] (contains? (:authors book) author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors))
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
