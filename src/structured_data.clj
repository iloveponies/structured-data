(ns structured-data)


(defn do-a-thing [x]
  (let [y (* 2 x)]
  (Math/pow y y))
  )

(defn spiff [v]
  (+ (get v 0)(get v 2))
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[x y z]v]
    (+ x z))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (- (first (second rectangle)) (first (first rectangle)))
  )

(defn height [rectangle]
  (- (second (second rectangle))(second(first rectangle)))
 )

(defn square? [rectangle]
  (if (= (height rectangle) (width rectangle)) true false)
  )

(defn area [rectangle]
   (* (height rectangle) (width rectangle))
  )

(defn contains-point? [rectangle point]
  (let [a (first(first rectangle))
        b (first(second rectangle))
        c (first point)
        d (second(first rectangle))
        f (second(second rectangle))
        g (second point)]
    (and
       (<= a c b)
       (<= d g f))
    )
  )

(defn contains-rectangle? [outer inner]
  (let [a (first(first outer))
        b (second(first outer))
        c (first(second outer))
        d (second(second outer))
        e (first(first inner))
        f (second(first inner))
        g (first(second inner))
        h (second(second inner))]
    (and
       (contains-point? [[a b][c d]] [e f])
       (contains-point? [[a b][c d]] [g h]))
    )
  )

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
  (let [a (:authors book)]
  (count a))
  )

(defn multiple-authors? [book]
  (if (< 1 (author-count book)) true false)
  )

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author))
  )

(defn alive? [author]
   (not (contains? author :death-year))
  )

(defn element-lengths [collection]
  (let [fix (fn[x] (count x))]
    (map fix collection))
  )

(defn second-elements [collection]
  (let [fix (fn[x] (get x 1))]
    (map fix collection))
  )

(defn titles [books]
  (let [everything books]
    (map :title everything))
  )

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq))
)

(defn stars [n]
  (apply str (repeat n "*"))
)

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
  )

(defn contains-duplicates? [a-seq]
  (let [a-set (set a-seq)]
    (not (== (count a-set)(count a-seq))))
  )

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
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
  (let [x (str (author :name))
        y (if (contains? author :birth-year)
            (str " (" (author :birth-year) " - " (author :death-year) ")"))]
  (str x y))
  )

(defn authors->string [authors]
  (let [x (set (map author->string authors))]
  (apply str (interpose ", " x)))
  )

(defn book->string [book]
  (str (book :title) ", written by " (authors->string (book :authors)))
  )

(defn books->string [books]
   (let [x (if (= 0 (count books)) (str "No books"))
         z (if (= 1 (count books)) (str "1 book. " ))
         y (if (< 1 (count books)) (str (count books) " books. "))
         w (apply str (interpose ", " (map book->string books)))]
   (str x z y w "."))
  )

(defn books-by-author [author books]
  (let [x (:book books)]
  (filter (fn[x] (has-author? x author)) books))
  )

(defn author-by-name [name authors]
  (let [x (:author authors)]
    (first (filter (fn[x] (= (:name x) name)) authors)))
  )

(defn living-authors [authors]
  (let [x (:author authors)]
    (filter (fn[x] (alive? x)) authors))
  )

(defn has-a-living-author? [book]
  (let [x (:authors book)
        y (set (living-authors x))]
  (not (empty? y)))
  )

(defn books-by-living-authors [books]
  (let [x (:book books)]
  (filter (fn[x](has-a-living-author? x)) books))
  )
