(ns structured-data)

(defn do-a-thing [x]
  (let [p (+ x x)]
    (Math/pow p p)
    )
  )

(defn spiff [v]
  (+ (get v 0) (get v 2))
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[x y z] v]
  (+ x z))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle x (- x1 x2)]
    (if (< x 0) (* -1 x) x)
  )
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle x (- y1 y2)]
    (if (< x 0) (* -1 x) x)
  )
  )

(defn square? [rectangle]
 (= (width rectangle) (height rectangle))
  )

(defn area [rectangle]
  (* (width rectangle) (height rectangle))
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [x y] point]
  (and (<= (min x1 x2) x (max x1 x2)) (<= (min y1 y2) y (max y1 y2)))
  )
  )

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner ]
  (and (contains-point? outer [x1 y1]) (contains-point? outer [x2 y2]))
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
 (let [v (:authors book) uusi (conj v new-author) p (assoc book :authors uusi)]
   p)
  )

(defn alive? [author]
  (not(contains? author :death-year))
  )

(defn element-lengths [collection]
  (let [asd (fn [a] (count a))]
    (map asd collection)
  )
  )

(defn second-elements [collection]
  (let [asd (fn [a] (get a 1))]
    (map asd collection)
  )
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
  (let[v (disj a-set elem) ]
    (if (> (count a-set) (count v)) v (conj a-set elem))

  )
  )

(defn contains-duplicates? [a-seq]
  (not(= (count (set a-seq)) (count a-seq))
  )
  )

(defn old-book->new-book [book]
  (let [v (:authors book) uusi (set v) p (assoc book :authors uusi)]
   p)
  )

(defn has-author? [book author]
  (contains? (:authors book) author)
  )

(defn authors [books]
  (apply clojure.set/union (map :authors books)
  )
  )

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (let [ n (:name author) b (:birth-year author) d (:death-year author)]
     (if (or b d) (str n " (" b " - " d ")") (str n))

    )
  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors) ))

  )

(defn book->string [book]
  (let [n (:title book) a (:authors book) w (str (authors->string a))]
  (str n ", written by " w)
    )
  )

(defn books->string [books]
  (let [m (fn [c](cond
   (= 0 (count c)) "No books"
   (= 1 (count c)) "1 book. "
   :else         (str (count c) " books. ")))
  l (apply str (m books) (interpose ". " (map book->string books)))]
    (str l ".")
  )
  )

(defn books-by-author [author books]
   (filter (fn [x] (has-author? x author)) books)
  )


(defn author-by-name [name authors]
(first(filter (fn [x]  (= (:name x) name)) authors))
  )

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors)
  )

(defn has-a-living-author? [book]
  (let [a (:authors book) b (living-authors a)]
  (not(empty? b))
    )
  )

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books)
  )

; %________%
