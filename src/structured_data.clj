(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)
    ))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn height [rectangle]
  (let[[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1))
  )

(defn width [rectangle]
  (let[[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1))
  )
(defn square? [rectangle]
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [
        [[x1 y1] [x2 y2]] rectangle
        [xp yp] point
       ]
    (and (and (>= xp x1)  (<= xp x2) (>= yp y1) (<= yp y2))
    )))

(defn contains-rectangle? [outer inner]
  (let[
       [down-left up-right] inner]
    (and (contains-point? outer down-left) (contains-point? outer up-right))
    )
  )

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc  book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [help (fn [coll] (get coll 1))]
  (map help collection))
  )

(defn titles [books]
  (let[helper (fn [book] (:title book))]
    (map helper books)
    )
  )

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))


(defn stars [n]
  (apply str(repeat n "*")))

(defn toggle [a-set elem]
    (if (contains? a-set elem)
     (disj a-set elem)
      (conj a-set elem))
   )

(defn contains-duplicates? [a-seq]
  (>  (count a-seq) (count (set a-seq)))
   )
(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (set(apply clojure.set/union (map :authors books))))

(defn all-author-names [books]
  (set (map :name (set(apply clojure.set/union (map :authors books))))))

(defn author->string [author]
  (let[name (:name author)
       date (cond
             (contains? author :death-year) (str " (" (:birth-year author) " - " (:death-year author) ")")
             (contains? author :birth-year) (str " (" (:birth-year author) " - " ")")
             :else "")]
  (str name date))
  )

(defn authors->string [authors]
 (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book)))
  )

(defn books->string [books]
  (cond
   (== (count books) 1) (str "1 book. " (apply str( interpose "," (map book->string books))) ".")
   (> (count books) 1) (str (count books) " books. " (apply str( interpose ", " (map book->string books)))".")
   :else "No books.")
  )

(defn books-by-author [author books]
  (filter (fn [x] (contains? (:authors x) author)) books))

(defn author-by-name [name authors]
  (first(filter (fn [x] (= (:name x) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors ))


(defn has-a-living-author? [book]
  (not(empty?(living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))
; %________%
