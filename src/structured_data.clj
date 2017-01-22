(ns structured-data)

(defn do-a-thing [x]
    (let [xx (+ x x)]
    (Math/pow (+ x x) (+ x x))
    )
)

(defn spiff [v]
  (+(get v 0)(get v 2))
)

(defn cutify [v]
  (conj v "<3")

)

(defn spiff-destructuring [v]
  (let [[x b c] v]
  (+ x c))
)

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
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (= 0 (- (- y2 y1) (- x2 x1))) true false)
  )
)

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1) )
  )
)

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x3 y3] point]
   (if (and (<= x1 x3 x2) (<= y1 y3 y2))true false)
  )

)

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
        [[x3 y3] [x4 y4]] inner]
   (if (and (contains-point? (rectangle [x1 y1] [x2 y2]) (point x3 y3))
            (contains-point? (rectangle [x1 y1] [x2 y2]) (point x4 y4))
      )true false)
  )
)

(defn title-length [book]
  (count(:title book))
)

(defn author-count [book]
  (count (:authors book))
)

(defn multiple-authors? [book]
  (if(< 1 (author-count book)) true false)
)

(defn add-author [book new-author]
  (assoc book :authors (conj (get book :authors) new-author))
)

(defn alive? [author]
  (not (contains? author :death-year))
)

(defn element-lengths [collection]
  (map count collection)
)

(defn second-elements [collection]
  (let [munge (fn [x] (get x 1))]
    (map munge collection))
)

(defn titles [books]
  (map :title books)
)

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq))
)

(defn stars [n]
  (apply str (repeat n "*"))
)

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
)

(defn contains-duplicates? [a-seq]
  (not (=(count (set a-seq)) (count a-seq)))
)

(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors)))
)

(defn has-author? [book author]
  (contains? (book :authors) author)
)

(defn authors [books]
   (let [author-names
         (fn [book] (:authors book))]
    (set (apply clojure.set/union (map author-names books))))
)

(defn all-author-names [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books))))
)

(defn author->string [author]
  (if (contains? author :birth-year) (str(:name author) " (" (:birth-year author)" - "(:death-year author) ")" ) (str(:name author)))
)

(defn authors->string [authors]
    (apply str (interpose ", " (map author->string authors)))
)

(defn book->string [book]
  (let [title (:title book)
    by ", written by "
    authors (authors->string (:authors book))]
    (str title by authors)
  )
)

(defn books->string [books]
  (let [number (count books)
        of " book"
        singular ". "
        plural "s. "
        books-and-authors
        (apply str
          (interpose ". "
            (map book->string books)))
        end "."]
    (cond
      (= 0 number) "No books."
      (= 1 number) (str number of singular books-and-authors end)
      :else (str number of plural books-and-authors end)))
)

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books)
)

(defn author-by-name [name authors]
  (first (filter  (fn [x] (= (:name x) name)) authors))

)

(defn living-authors [authors]
  (filter  (fn [x] (alive? x)) authors)
)

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))
)

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
)

; %________%
