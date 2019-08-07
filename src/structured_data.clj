(ns structured-data)

(defn do-a-thing [x]
  (let [plusx (+ x x)] (Math/pow plusx plusx )))

(defn spiff [v]
  (+(get v 0)(get v 2)))


(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (+(get v 0)(get v 2))
        )

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
   (if (= (height rectangle) (width rectangle) )true false)
   )

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle ]
    (let [[x3 y3] point ]
           (if (and
              (<= x1 x3 x2)
              (<= y1 y3 y2))
              true false)
      ))
      )


(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]]  inner ]
    (contains-point? outer [x2 y2]
      )
    )
  )


(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if(>=  (author-count book) 2)true false
      )
  )


(defn add-author [book new-author]
   (assoc book :authors (conj (:authors book) new-author)))


(defn alive? [author]
   (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [ x (fn [y] (get y 1)) ]
  (map x collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not(= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book ))))


(defn has-author? [book author]
  (contains? (:authors book) author))

;(defn all-author-names [books]
;(let [author-names
;       (fn [book] (map :name (:authors book)))]
;  (set (apply concat (map author-names books)))))

(defn authors [books]
    (apply clojure.set/union (map :authors (map old-book->new-book books))))


(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
   (let [ life (fn [x]
    (cond
     (:death-year x) (str " (" (:birth-year x) " - " (:death-year x) ")")
     (:birth-year x) (str " (" (:birth-year x) " - )")
     :else ""))]
 (str (:name author) (life author))))

;(defn authors->string [authors]
 ; (apply str (interpose ", " (map author->string authors))))

(defn authors->string [authors]
   (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book)))
  )

(defn books->string [books]
  (cond (>= 0  (count books))
        "No books."
        (== 1 (count books))
        (str "1 book. " (book->string (first books)) ".")
        :else
        ( str (count books) " books. " (apply str (interpose ". " (map book->string books)))".")))



;=> "1 book. The City and the City, written by China Miéville (1972 - )."

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
   (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
