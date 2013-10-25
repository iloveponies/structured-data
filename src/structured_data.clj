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
  :-)

(defn stars [n]
  (apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
  :-)

(defn contains-duplicates? [a-seq]
  :-)

(defn old-book->new-book [book]
  :-)

(defn has-author? [book author]
  :-)

(defn authors [books]
  :-)

(defn all-author-names [books]
  :-)

(defn author->string [author]
  :-)

(defn authors->string [authors]
  :-)

(defn book->string [book]
  :-)

(defn books->string [books]
  :-)

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%





