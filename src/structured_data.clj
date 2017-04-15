(ns structured-data)

(defn do-a-thing [x]
  (let [xpx (+ x x)]
    (Math/pow xpx xpx))
)

(defn spiff [v]
  (+ (get v 0) (get v 2))
  )

(defn cutify [v]
  (conj v "<3")
)

(defn spiff-destructuring [[x y z]]
  (+ x z)
)

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1) 
)

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1)
)

(defn square? [[[x1 y1] [x2 y2]]]
  (if (= (- x2 x1) (- y2 y1))
    true
    false )
)

(defn area [[[x1 y1] [x2 y2]]]
  (* (- x2 x1) (- y2 y1))
)

(defn contains-point? [[[x1 y1] [x2 y2]] [px py]]
  (if (and (<= x1 px x2) (<= y1 py y2))
    true
    false )
)

(defn contains-rectangle? [[[ox1 oy1] [ox2 oy2]] [[ix1 iy1] [ix2 iy2]]]
  (if (and (<= ox1 ix1 ix2 ox2) (<= oy1 iy1 iy2 oy2))
    true
    false )
)

(defn title-length [book]
  (count (get book :title ) )
)

(defn author-count [book]
  (count (get book :authors))
)

(defn multiple-authors? [book]
  (if (> (count (get book :authors)) 1)
    true
    false
  )
)

(defn add-author [book new-author]
  (let [new-authorlist (conj (get book :authors) new-author) ]
    (assoc book :authors new-authorlist))
)

(defn alive? [author]
  (if (contains? author :death-year)
    false 
    true )
)

(defn element-lengths [collection]
  (map count collection)
)

(defn second-elements [collection]
  (let [myfun (fn [x] (get x 1))]
    (map myfun collection)
  )
)

(defn titles [books]
  (map :title books)
)

(defn monotonic? [a-seq]
  (if (or (apply >= a-seq) (apply <= a-seq))
    true
    false )
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
  (if (> (count a-seq) (count (set a-seq)))
    true
    false )
)

(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors)))
)

(defn authors [books]
  (apply clojure.set/union (map :authors books)) 
)

(defn all-author-names [books]
  (set (map :name (authors books)))
)

(defn author->string [author]
  (let [name (get author :name) 
        byear (get author :birth-year)
        dyear (get author :death-year) ]
      (cond (and (contains? author :birth-year) (contains? author :death-year)) (str name " (" byear " - " dyear ")")
            (contains? author :birth-year)   (str name " (" byear " - )")
            :else (str name)))
)

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
)

(defn book->string [book]
  (str (get book :title) ", written by " (authors->string (get book :authors)))
)

(defn books->string [books]
  (let [bookcount (count books)]
  (cond (= bookcount 1 ) (str "1 book. " (apply book->string books) ".")
        (> bookcount 1 ) (str bookcount " books. " (apply str (map book->string books)) ".")
        :else (str "No books.") ))
)
(defn has-author? [book author]
  (if (contains? (get book :authors) author)
    true
    false )
)


(defn books-by-author [author books]
  (let [filted (filter (fn [bookk] (has-author? bookk author)) books)]
    filted)
)


(defn author-by-name [name authors]
  (let [filted (filter (fn [x] (= name (:name (first x)))) authors)]
    (first filted))
)

(defn living-authors [authors]
  (filter alive? authors)
)

(defn has-a-living-author? [book]
  (let [livelist (filter alive? (:authors book)) ]
    (if (empty? livelist)
      false 
      true))
)

(defn books-by-living-authors [books]
  () 
)

; %________%
