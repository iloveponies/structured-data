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
    (+ x z)
  )
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
    (
      if (== (- x2 x1) (- y2 y1))
      true
      false
    )
  )
)

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))
  )
)

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xt yt] point]
    (
      if (and (<= x1 xt x2) (<= y1 yt y2))
      true
      false
    )
  )
)

(defn contains-rectangle? [outer inner]
  (let [[[xo1 yo1] [xo2 yo2]] outer
        [[xi1 yi1] [xi2 yi2]] inner]
    (
      if (and (contains-point? outer [xi1 yi1]) (contains-point? outer [xi2 yi2]))
      true
      false
    )
  )
)

(defn title-length [book]
  (count (:title book))
)

(defn author-count [book]
  (count (:authors book))
)

(defn multiple-authors? [book]
  (> (author-count book) 1)
)

(defn add-author [book new-author]
  (let [v (:authors book)]
    (assoc book :authors (conj v new-author))
  )
)

(defn alive? [author]
  (
    if (contains? author :death-year)
    false
    true
  )
)

(defn element-lengths [collection]
  (defn mungea [x]
    (count x))
  (map mungea collection)
)

(defn second-elements [collection]
  (let [mungeb (fn [x] (get x 1))]
    (map mungeb collection))
)

(defn titles [books]
  (map :title books)
)

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq))
    true
    false
  )
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
  (if (< (count (set a-seq)) (count a-seq))
    true
    false
  )
)

; (def books [cities, wild-seed, embassytown, little-schemer])

; :authors [china] --> :authors #{china}

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
)

(defn has-author? [book author]
  (if (contains? (:authors book) author)
    true
    false
  )
)

(defn authors [books]
  (apply clojure.set/union (map :authors books))
)

(defn all-author-names [books]
  (set (map :name (set (authors books))))
)

(defn authoryear->string [author]
  (if (contains? author :death-year)
    (str "(" (:birth-year author) " - " (:death-year author) ")")
    (if (contains? author :birth-year)
      (str "(" (:birth-year author) " - )")
      (str "")
    )
  )
)

(defn author->string [author]
  (
    if (> (count (authoryear->string author)) 0) 
      (str (:name author) " " (authoryear->string author))
      (str (:name author))
  )
)

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
)

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book)))
)

(defn books->string [books]
  (
   if (== (count books) 1)
      (str (str 1 " book. ") 
       (apply str (interpose ". " (map book->string books)))
       ".")
      (if (== (count books) 0)
         (str (str "No" " books.") 
       (apply str (interpose ". " (map book->string books))))
         (str (str (count books) " books. ") 
       (apply str (interpose ". " (map book->string books))) 
       ".")
      )
  )
  ; (str (str (count books) " books.") 
  ;      (apply str (interpose ". " (map book->string books))) 
  ;      ".")
)

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books)
)

(defn author-by-name [name authors]
  ; (first (filter (fn [x] (= name x)) (set (map :name (set authors)))))
  (first (filter (fn [x] (= (str (:name x)) name)) (set authors)))
)

(defn living-authors [authors]
  (filter alive? (set authors))
)

(defn has-a-living-author? [book]
 (if (empty? (filter alive? (:authors book)))
   false
   true
 )
)

(defn books-by-living-authors [books]
  (filter has-a-living-author? (set books))
)

; %________%
