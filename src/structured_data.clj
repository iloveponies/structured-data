(ns structured-data)


(defn do-a-thing [x]
  (let [xx (+ x x)]
  (Math/pow xx xx))
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
  (let[[[x1 y1] [x2 y2]] rectangle]
    (cond
       (< x1 x2) (- x2 x1)
       :else (- x1 x2)
     )
    )
 )

(defn height [rectangle]
   (let[[[x1 y1] [x2 y2]] rectangle]
    (cond
       (< y1 y2) (- y2 y1)
       :else (- y1 y2)
     )
    )
  )

(defn square? [rectangle]
  (let[[[x1 y1] [x2 y2]] rectangle]
    (if (= (- x1 y1) (- x2 y2)) true false))
  )

(defn abs [x]
  (cond
   (< x 0) (* x -1)
   :else x)
  )

(defn area [rectangle]
   (let[[[x1 y1] [x2 y2]] rectangle]
     (abs (* (- x1 x2) (- y1 y2))))
  )


(defn contains-point? [rectangle point]
 (let[[[x1 y1] [x2 y2] ]rectangle]
   (let[[x3 y3] point]
  (if (or(and (<= x1 x3 x2) (<= y1 y3 y2)) (and (<= x2 x3 x1) (<= y2 y3 y1))) true false)
     )
   )
  )


(defn contains-rectangle? [outer inner]
  (let[[[x1 y1] [x2 y2]] outer]
    (let[[[x3 y3] [x4 y4]] inner]
      (cond
       (or (> x1 x3) (> y1 y3)) false
       (or  (< x2 x4) (< y2 y4)) false
       :else true
       )))
  )


(defn title-length [book]
  (count (get book :title))
  )


(defn author-count [book]
  (count (get book :authors))
  )


(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false)
  )


(defn add-author [book new-author]
 (let[[]new-author]
     (assoc book :authors (conj (get book :authors) new-author))
  )
  )



(defn alive? [author]
  (if (get author :death-year) false true)
  )


(defn element-lengths [collection]
 (let [pituus (fn [x] (count x))]
  (map pituus collection)))


(defn second-elements [collection]
   (let [tokat (fn[x] (get x 1))]
     (map tokat collection)))


(defn titles [books]
  (let [titlet (fn[x] (get x :title))]
    (map titlet books)))

(defn monotonic? [a-seq]
   (or (apply <= a-seq) (apply >= a-seq))
  )

(defn stars [n]
  (apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
   (if (contains? a-set elem)
     (disj a-set elem)
     (conj a-set elem))
  )

(defn contains-duplicates? [a-seq]
  (if (== (count (set a-seq)) (count a-seq)) false true)
  )

(defn old-book->new-book [book]
  (assoc book :authors(set(get book :authors))))


(defn has-author? [book author]
  (contains? (get book :authors) author)
  )

(defn authors [books]
  (apply clojure.set/union (map :authors books))
  )


(defn all-author-names [books]
   (set (map :name (authors books)))
  )


(defn author->string [author]
  (let [nimi (:name author)
        vuodet
        (cond
         (contains? author :death-year) (str " (" (:birth-year author) " - " (:death-year author) ")")
         (contains? author :birth-year) (str " (" (:birth-year author) " - )")
         :else (str ""))
        ]
    (str nimi vuodet)
    )
  )

(defn authors->string [authors]
  (apply str (interpose ", " ( map author->string authors)))
  )


(defn book->string [book]
 (let [nimi (:title book)]
   (str nimi ", written by " (authors->string (:authors book))))
  )

(defn books->string [books]
  (cond
   (= (count books) 0) (str "No books.")
   (= (count books) 1) (str(count books) " book. "(apply str (interpose ". " (map book->string books ) ) ) ".")
   :else (str(count books) " books. "(apply str (interpose ". " (map book->string books ) ) ) ".")
   )
  )

(defn books-by-author [author books]
   (filter (fn[book] (has-author? book author)) books)
  )

(defn author-by-name [name authors]
   (first(filter (fn[author] (= name (:name author)))authors))
  )

(defn living-authors [authors]
  (filter (fn[author] (alive? author))authors)
  )


(defn has-a-living-author? [book]
    (if ( < 0 (count (living-authors (:authors book)))) true false)
  )


(defn books-by-living-authors [books]
  (filter (fn[book] (has-a-living-author? book) )books)
  )


; %________%









