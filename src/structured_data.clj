(ns structured-data)

(defn do-a-thing [x]
  (let [thing (+ x x)]
  (Math/pow thing thing)
  ))




(defn spiff [v]
  (let [first (get v 0)
        third (get v 2)]
  (+ first third)
  ))


(defn cutify [v]
  (conj v "<3")
  )


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
  (+ (* (width rectangle) (height rectangle)) 0)
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
  (count (:title book))
  )

(defn author-count [book]
  (count (:authors book))
  )



(defn multiple-authors? [book]
  (if (> (author-count book) 1)
    true
    false
    ))


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
  (let [sec (fn [x] (get x 1))]
  (map sec collection)
  ))



(defn titles [books]
  (map :title books)
  )



(defn monotonic? [a-seq]
    (let [monoD (fn [x] (apply <= a-seq))
          monoI (fn [x] (apply >= a-seq))]
      (or (monoD a-seq) (monoI a-seq))
  ))

(defn stars [n]
  (apply str (repeat n \*))
  )




(defn toggle [a-set elem]
  (cond
   (contains? a-set elem) (disj a-set elem)
   :else                  (conj a-set elem)
  ))


(defn contains-duplicates? [a-seq]
  (let [setted (set a-seq)]
  (not (== (count setted) (count a-seq)))
  ))


(defn old-book->new-book [book]
  (assoc book :authors (set (book :authors)))
  )



(defn has-author? [book author]
  (contains? (book :authors) author)
  )



(defn authors [books]
  (apply clojure.set/union (map :authors books))
  )




(defn all-author-names [books]
  (set (map :name (authors books)))
  )


(defn author->string [author]
  (let [name (str (:name author))
        years (str (:birth-year author)  " - "(:death-year author))]
    (cond
     (contains? author :death-year) (str name " (" years ")")
     (contains? author :birth-year) (str name " (" years ")")
     :else (str name)
  )))



(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )



(defn book->string [book]
  (let [name (str (book :title))]
  (str name ", written by " (authors->string (:authors book)))
  ))



(defn books->string [books]

  (cond
   (== (count books) 0) (str "No books.")
   (== (count books) 1) (str (apply str (count books) " book. " (interpose ". " (map book->string books)))".")
   :else                (str (apply str (count books) " books. " (interpose ". " (map book->string books)))".")
  ))



(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books)
  )



(defn author-by-name [name authors]
    (first (filter (fn [x] (= name (:name x))) authors))
  )


(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors)
  )


(defn has-a-living-author? [book]
    (not(empty? (living-authors (book :authors))))
  )



(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )


; %________%
