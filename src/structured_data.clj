(ns structured-data)

(defn do-a-thing [x]
  (let [z (+ x x)]
    (Math/pow z z)))

(defn spiff [v]
  (+ (get v 0) (get v 2))
  )

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (- x2 x1)))


(defn height [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle))
  )

(defn area [rectangle]
  (* (width rectangle) (height rectangle))
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle
        [p1 p2] point]
    (and
      (<= x1 p1 x2)
      (<= y1 p2 y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and
      (contains-point? outer p1)
      (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [authors (:authors book)]
  (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-sec (fn [x] (get x 1))]
    (map get-sec collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)))

(defn stars [n]
  (apply str
    (repeat n \*)
  )
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem)
  (disj a-set elem)
  (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (=
         (count a-seq)
         (count (set a-seq))
         )
       )
  )

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (set (apply concat (map :authors books))))


(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let[name (:name author)
       bday (:birth-year author)
       dday (:death-year author)]
    (if bday
      (str name " (" bday " - " dday ")")
      (str name))
    )
  )

(defn authors->string [authors]
  (let [formauthors (map author->string authors)]
  (apply str(interpose ", " formauthors))))

(defn book->string [book]
  (let [title (:title book)
        authors (authors->string (:authors book))]
    (str title ", written by " authors))
    )

(defn books->string [books]
  (let [authors (apply str (interpose ", " (map book->string books)))
        book_count (count books)]
    (if (> book_count 0)
      (if (= book_count 1)
        (str "1 book. " authors ".")
        (str book_count " books. " authors "."))
      (str "No books.")
      )
    )
  )

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
 (let [matched (filter (fn [author] (= name (:name author))) authors)]
     (first matched))
  )


(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [auts (:authors book)]
  (> (count (living-authors auts)) 0)
  )
  )

(defn books-by-living-authors [books]
  (filter (fn [b] (has-a-living-author? b)) books))

; %________%
