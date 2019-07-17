(ns structured-data)




(defn do-a-thing [x]
 (let [DoubleX (+ x x)]
  (Math/pow DoubleX DoubleX)))



(defn spiff [v]
  (+ (get v 2) (get v 0)))



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

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (- x2 x1)
  ))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (- y2 y1)
  ))




(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (== 0 (- (- x1 x2) (- y1 y2)))
  ))


(defn area [rectangle]
   (+ (* (height rectangle) (width rectangle)) 0)
  )



(defn contains-point? [rectangle point]
   (let [[[x1 y1] [x2 y2]] rectangle
         [x y] point]
  (and (<= x1 x x2) (<= y1 y y2))
  ))



(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
        [[x3 y3] [x4 y4]] inner]
    (and
  (contains-point? outer (point x3 y3))
  (contains-point? outer (point x4 y4))
  )))





(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))


(defn multiple-authors? [book]
  (> (author-count book) 1)
  )



(defn add-author [book new-author]
  (let [{title :title, author :authors} book]
   (assoc book :authors (conj author new-author))
   ))


(defn alive? [author]
 (not (contains? author :death-year))
  )


(defn element-lengths [collection]
  (map count collection)
  )


(defn second-elements [collection]
  (let [getSecond (fn [x] (get x 1))]
    (map getSecond collection)
 ))

(defn titles [books]
  (map :title books)
  )


(defn monotonic? [a-seq]
  (let [testMonoI (fn [x] (apply <= x))
       testMonoD (fn [x] (apply >= x))]
    (cond
     (testMonoI a-seq) true
     (not (testMonoI a-seq)) (testMonoD a-seq)
     :else             false
     )
  ))



(defn stars [n]
  (let [printStars (fn [x] (repeat x "*"))]
  (apply str (printStars n))
  ))



(defn toggle [a-set elem]
  (cond
     (contains? a-set elem) (disj a-set elem)
     (not (contains? a-set elem)) (conj a-set elem)
   )
  )




(defn contains-duplicates? [a-seq]
   (< (count (set a-seq)) (count a-seq))
  )



(defn old-book->new-book [book]
  (let [{title :title, author :authors} book]
    (assoc book :authors (set author))
  ))




(defn has-author? [book author]
  (contains? (book :authors) author)

  )


(defn authors [books]
   (apply clojure.set/union (map :authors books))
  )



(defn all-author-names [books]
  (set(map :name (authors books)))
  )


(defn author->string [author]
  (let [{name :name bYear :birth-year dYear :death-year} author]
  (cond
      (not(alive? author)) (str name " (" bYear " - " dYear")")
      (contains? author :birth-year) (str name " (" bYear " - " dYear")")
      :else                          (str name)
  )))




(defn authors->string [authors]
  (apply str(interpose ", "(map author->string authors)))
  )




(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book)))
  )


(defn books->string [books]
  (cond
  (== (count books) 0) "No books."
  (> (count books) 1) (str (apply str (count books) " books. " (interpose ". " (map book->string books)))".")
  :else               (str (apply str (count books) " book. " (interpose ". " (map book->string books)))".")
  ))



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
  (not(empty?(living-authors (book :authors))))
  )


(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )


; %________%
