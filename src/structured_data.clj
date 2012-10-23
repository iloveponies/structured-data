(ns structured-data)

(defn do-a-thing [x]
  (let [sq (+ x x)]
    (Math/pow sq sq) )
  )

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
  (if (= (width rectangle) (height rectangle)) true false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle
        [x y] point]
	(if (and (and (<= x x2) (>= x x1)) (and (<= y y2) (>= y y1))) 
    	true
    	false
    )))

(defn contains-rectangle? [outer inner]
  (let [[[ox1 oy1][ox2 oy2]] outer
        [[ix1 iy1][ix2 iy2]] inner]

	(if (and (contains-point? outer [ix1 iy1]) 
             (contains-point? outer [ix2 iy2])) 
      true false)
    ))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
	(count (get book :authors)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
    (assoc book :authors (conj (get book :authors) new-author)))

(defn alive? [author]
  (if (= (get author :death-year) nil) true false))

(defn element-lengths [collection]
  (let [ce (fn [x] (count x))]
	(map ce collection)))

(defn second-elements [collection]
  (let [se (fn [x] ( second x ))]
    (map se collection)))

(defn titles [books]
  (let [ti (fn [x] ( get x :title ))]
    (map ti books)))

(defn monotonic? [a-seq]
  (cond
   	(apply <= a-seq) true
   	(apply >= a-seq) true
   	:else false
   ))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (let [s (count a-set)
        y (count (conj a-set elem))]
    (if (> y s) 
      (conj a-set elem)
      (disj a-set elem)
      )))

(defn contains-duplicates? [a-seq]
  (let [as (count a-seq)
		ss (count (set a-seq))]
    (if (> as ss) true false)))

(defn old-book->new-book [book]
	(assoc book :authors (set (get book :authors)))
  )

(defn has-author? [book author]
  (contains? (get (old-book->new-book book) :authors) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books )))

(defn all-author-names [books]
   (set (map :name (authors books))))

(defn author->string [author]
  (let [an (:name author)
        by (:birth-year author)
        dy (:death-year author) ]
    (cond 
     	(= by nil) (str an)
     	:else (str an " (" by " - " dy ")")
     )))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)) ))

(defn book->string [book]
  (let [bname (get book :title)
        authors (authors->string (authors [book]))]
    (str bname ", written by " authors)))

(defn books->string [books]
  (let [bc (count books)
        bl (apply str (interpose ". " (map book->string books)))]
    (cond 
     	(> bc 1) (str bc " books. " bl ".")
     	(= bc 1) (str bc " book. " bl ".")
     	:else 	(str "No books.")
     )))

(defn books-by-author [author books]
  (filter (fn [x] ( has-author? x author)) books))

(defn author-by-name [name authors]
  (let [auth (filter (fn [x] (if (= (:name x) name) true false)) authors)]
    (if (> (count auth) 0) (first auth) nil)))

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))

(defn has-a-living-author? [book]
  (if (> (count(filter (fn [x] (alive? x)) (authors [book]))) 0) 
    true false))

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))