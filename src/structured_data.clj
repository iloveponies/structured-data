(ns structured-data)

(defn do-a-thing [x]
(
 let[x (+ x x)]
 (Math/pow x x)
 )
  )

(defn spiff [v]
 (if (>= (count v) 3)
  (+ (get v 0) (get v 2))
   "?"
   )
  )

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
 (if (>= (count v) 3)
  (let [[x y z] v] (+ x z))
   "?"
   )
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
(- (get (get rectangle 1) 0) (get (get rectangle 0) 0))
  )

(defn height [rectangle]
(- (get (get rectangle 1) 1) (get (get rectangle 0) 1))
  )

(defn square? [rectangle]
 (if (= (width rectangle) (height rectangle))
    true
    false
   )
  )

(defn area [rectangle]
(* (width rectangle) (height rectangle))
  )

(defn contains-point? [rectangle point]
  (if (and (<= (get (get rectangle 0) 0) (get point 0) (get (get rectangle 1) 0)) 
      (<= (get (get rectangle 0) 1) (get point 1) (get (get rectangle 1) 1)))
   true
   false
    )
  )

(defn contains-rectangle? [outer inner]

  (if (and (and (<= (get (get outer 0) 0) (get (get inner 0) 0) (get (get outer 1) 0)) 
      (<= (get (get outer 0) 1) (get (get inner 0) 1) (get (get outer 1) 1)))
      (and (<= (get (get outer 0) 0) (get (get inner 1) 0) (get (get outer 1) 0)) 
      (<= (get (get outer 0) 1) (get (get inner 1) 1) (get (get outer 1) 1))))  
   true
   false
    )
  )

(defn title-length [book]
(count (get book :title))
  )

(defn author-count [book]
(count (:authors book))
  )

(defn multiple-authors? [book]
 (if (> (count (:authors book)) 1)
   true
  false)
  )

(defn add-author [book new-author]
(assoc book :authors (conj (:authors book) new-author))
  )

(defn alive? [author]
 (if (contains? author :death-year)
  false
   true
   )
  )

(defn element-lengths [collection]
(map count collection)
  )

(defn second-elements [collection]
 (let [y (fn [x] (get x 1))]
    (map y collection))
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
     (conj a-set elem))
  )

(defn contains-duplicates? [a-seq]
 (let [b-seq (set a-seq)] 
  (if (= (count b-seq) (count a-seq))
  false
    true
    ))
  )

(defn old-book->new-book [book]
 (let [authors (get book :authors)]
  (assoc book :authors (set authors))) 
  )

(defn has-author? [book author]
(if (contains? (get book :authors) author)
  true
  false
  )
  )

(defn authors [books]
(apply clojure.set/union (map :authors books))

  )

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (let [name (get author :name)
        birth (get author :birth-year)
        death (get author :death-year)
        ] 
    (if (get author :birth-year) (apply str name " (" birth " - " death ")") (str name)))
  )

(defn authors->string [authors]
      (apply str (interpose "," (map author->string authors)))
  )

(defn book->string [book]
     (let [title (apply str (get book :title))
      author (authors->string (get book :authors))]
     (apply str title ", written by " author))
  )

(defn books->string [books]
  (let [n (count books)]
    (cond
       (= n 0) "No books."
        (= n 1) (str (apply str n " book. " (interpose ". " (map book->string books))) ".")
       :else (str (apply str n " books. " (interpose ". " (map book->string books))) ".")
     )
    )
)

(defn books-by-author [author books]
    (let [lambda (fn [book] (has-author? book author))]
     (filter lambda books))
  )

(defn author-by-name [name authors]
(first (filter (fn [x] (boolean (= name (:name x)))) authors))
  )

(defn living-authors [authors]
  (filter alive? authors)
  )

(defn has-a-living-author? [book]
   (let [authors (get book :authors)]
  (> (count (living-authors authors)) 0))
  )

(defn books-by-living-authors [books]
   (filter has-a-living-author? books)
  )