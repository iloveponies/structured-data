(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)] (Math/pow xx xx)))

(defn spiff [v]
(cond (> (count v) 2)
  (+ (get v 0) (get v 2))
  :else \?))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
  (+ x z))
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
     (= (- y2 y1) (- x2 x1))
     )
  )

(defn area [rectangle]

   (let [[[x1 y1] [x2 y2]] rectangle]
     (* (- y2 y1) (- x2 x1))
     )
  )

(defn contains-point? [rectangle point]
(let [ [rect1 rect2] rectangle [x1 y1] rect1 [x2 y2] rect2 [x y] point ]
(and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
(let [ [point1 point2] inner ]
(and (contains-point? outer point1) (contains-point? outer point2))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
(count (get book :authors))
  )

(defn multiple-authors? [book]
(> (count (get book :authors)) 1)
  )

(defn add-author [book new-author]
(assoc book :authors (conj (:authors book) new-author))
  )

 (defn alive? [author]
(not (contains? author :death-year))
   )

(defn element-lengths [collection]
(map count collection)
  )

(defn second-elements [collection]
  (let [munge (fn [x] (get x 1))]
 (map munge collection)
  ))

(defn titles [books]
(map :title books)
  )

(defn monotonic? [a-seq]
(or(apply <= a-seq) (apply >= a-seq))
  )


(defn stars [n]
  (apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
(if(contains? a-set  elem)
     (disj  a-set  elem)
    (conj a-set  elem)
  )
  )

(defn contains-duplicates? [a-seq]
  (not(= (count a-seq) (count (set a-seq))))
)

(defn old-book->new-book [book]
(assoc book :authors (set (:authors book))))

(defn has-author? [book author]
    (contains? (:authors book ) author)
  )


(defn authors [books]
  (let [bk (fn [book] (:authors book))]
  (set (apply concat (map bk books)))
  ))


 (defn all-author-names [books]

  (set (map :name (authors books)))
   )


 (defn author->string [author]
  ;(:name author)
   ;(nil?(:birth-year author))
   ;(str(:birth-year author))
   (let [{:keys [name birth-year death-year]} author]
     ;(format "%s | %s " name birth-year)
     (if (> (count (str birth-year)) 0)
     (str name " (" birth-year " - " death-year ")")
       (str name)
     )
   ))

 (defn authors->string [authors]
   (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
(str (:title book) ", written by " (authors->string (:authors book)))

)

(defn books->string [books]
 (if (= (count books) 0)
   (str "No books.")
   (
    str (count books) (if (= (count books) 1)
                        " book"
                        " books"
                        )
                        ". "
    (apply str (interpose ". "(map book->string books)))
    ".")
   )
  )

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
