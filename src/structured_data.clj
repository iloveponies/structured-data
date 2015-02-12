2(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)
  )
)

(defn spiff [v]
  (if(>= (count v) 3) (+ (get v 0) (get v 2)) "?")
)

(defn cutify [v]
  (conj v "<3")
)

(defn spiff-destructuring [v]
  (if(>= (count v) 3)
    (let [[x y z ] v] (+ x z)) "?")
)

(defn point [x y]
  [x y]
)

(defn rectangle [bottom-left top-right]
  [bottom-left top-right]
)

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
  (if(== (width rectangle) (height rectangle)) true false)
)

(defn area [rectangle]
  (* (width rectangle) (height rectangle))
)

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[px py] point]
      (if(and
        (>= px x1)
        (>= py y1)
        (<= px x2)
        (<= py y2)
        ) true false
      )
    )
  )
)

(defn contains-rectangle? [outer inner]
  (let [[[i1x i1y] [i2x i2y]] inner]
    (if (and
       (contains-point? outer (point i1x i1y))
       (contains-point? outer (point i2x i2y))
      ) true false
    )
  )
)

(defn title-length [book]
  (count (book :title))
)

(defn author-count [book]
  (count(book :authors))
)

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false)
)

(defn add-author [book new-author]
  (let [current (book :authors)]
    (let [new (conj current new-author)]
      (assoc book :authors new )
    )
  )
)

(defn alive? [author]
  (if (nil? (author :death-year)) true false)
)

(defn element-lengths [collection]
  (map (fn [x] (count x)) collection))

(defn second-elements [collection]
  (map
    (fn [coll]
      (let [[a b c] coll] b)
   ) collection)
)

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq))
)

(defn stars [n]
  (apply str (repeat n "*"))
)

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
  )
)

(defn contains-duplicates? [a-seq]
  (if (< (count (set a-seq)) (count a-seq)) true false)
)

(defn old-book->new-book [book]
  (assoc book :authors (set (book :authors)))
)


(defn has-author? [book author]
  (if (contains? (book :authors) author) true false)
)

(defn authors [books]
  (apply clojure.set/union (map :authors books))
)

(defn all-author-names [books]
  (set (map :name (authors books)))
)

(defn author->string [author]
  (str
;   (author :name)
;   (if (nil? (author :birth-year)) "" (str " (" (author :birth-year) " - "))
;   (if (nil? (author :death-year)) "" (author :death-year))
;   (if (nil? (author :birth-year)) "" ")" )
   (let [name (author :name)] name)
   (if (nil? (author :birth-year))
     ""
     (str
       (let [born (author :birth-year)] (str " (" born " - "))
       (let [died (author :death-year)] (str died ")"))))))

(defn authors->string [authors]
  (apply str(interpose ", " (map (fn [author] (author->string author)) authors)))
)

(defn book->string [book]
  (str
   (book :title) ", written by "
   (authors->string (book :authors))
  )
)

(defn books->string [books]
  (if (empty? books)
      "No books."
      (str (count books) (if (> (count books) 1) " books. " " book. ")
        (apply str (interpose ", " (map (fn [book] (book->string book)) books))) ".")
  )
)

(defn books-by-author [author books]
  (filter (fn [book] (contains? (book :authors) author) ) books)
  )


(defn author-by-name [name authors]
  (first (filter (fn [author] (if (= (author :name) name) author nil)) authors))
)

(defn living-authors [authors]
  (filter (fn [author] (if (nil? (author :death-year)) author nil)) authors)
)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
