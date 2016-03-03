(ns structured-data)

(defn do-a-thing [x]
  (let [sum (+ x x)]
    (Math/pow sum sum)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

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
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (== (- x2 x1) (- y2 y1)) true false)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
       [x3 y3] point]
    (if (and (<= x1 x3 x2) (<= y1 y3 y2)) true false)
    ))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
       [[x3 y3] [x4 y4]] inner]
    (if (and (<= x1 x3 x2) (<= y1 y3 y2) (<= x1 x4 x2) (<= y1 y4 y2)) true false)
  ))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (== 1 (count (:authors book))) false true))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (if (contains? book :title) (assoc book :authors (conj authors new-author)) (assoc book :authors (conj authors new-author)))
    ))

(defn alive? [author]
  (if (contains? author :death-year) false true)
  )

(defn element-lengths [collection]
  (let [munge (fn [x] (count x))]
    (map munge collection)))

(defn second-elements [collection]
  (let [munge (fn [x] (get x 1))]
    (map munge collection))
  )

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq))
  )

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
    )

(defn contains-duplicates? [a-seq]
  (if (== (count a-seq) (count (set a-seq))) false true)
  )

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
        birth (:birth-year author)
        death (:death-year author)]
    (cond
      (contains? author :death-year)
        (str name " (" birth " - " death ")")
      (contains? author :birth-year)
        (str name " (" birth " - )")
      :else (str name))
  ))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (let [title (:title book)
        authors (authors->string (:authors book))]
    (str title ", written by " authors))
  )

(defn books->string [books]
  (let [nrOfBooks (count books)
        theBooks (apply str (interpose ". " (map book->string books)))]
    (cond
      (== 0 nrOfBooks) (str "No books.")
      (== 1 nrOfBooks) (str "1 book. " theBooks ".")
      :else            (str nrOfBooks " books. " theBooks ".")
      )
    )
  )

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books)
  )

(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (:name x))) authors))
  )

(defn living-authors [authors]
  (filter alive? authors)
  )

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))
  )

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books)
  )

; %________%
