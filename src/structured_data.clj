(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
(Math/pow xx xx
)))

(defn spiff [v]
  (let [a (get v 0)
        b (get v 2)]
   (+ a b)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
 (let [[x y] [(get v 0) (get v 2)]]
(+ x y)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[x y] [(get rectangle 0)
	(get rectangle 1)]]
(- (get y 0) (get x 0))))

(defn height [rectangle]
  (let [[x y] [(get rectangle 0)
		(get rectangle 1)]]
(- (get y 1) (get x 1))))

(defn square? [rectangle]
  (if (== (width rectangle) (height rectangle))
	true
	false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
    (let [[x y] [(get rectangle 0)
                 (get rectangle 1)
                 ]]
  (and (<= (get x 0) (get point 0) (get y 0))
       (<= (get x 1) (get point 1) (get y 1)))))

(defn contains-rectangle? [outer inner]
  (and (contains-point? outer (get inner 0))
       (contains-point? outer (get inner 1))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
   (> (author-count book) 1))

(defn add-author [book new-author]
 (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
(if (contains? author :death-year)
false
true
  ))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
 (let [element (fn [trollo] (get trollo 1))]
  (map element collection)))

(defn titles [books]
   (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
 (apply str (repeat n "*") ))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
	(disj a-set elem)
	(conj a-set elem)
))

(defn contains-duplicates? [a-seq]
  (if (== (count a-seq) (count (set a-seq)))
	false
	true))

(defn old-book->new-book [book]
  (assoc book :authors 
  (set (:authors book))))

(defn has-author? [book author]
  (if (contains? (:authors book) author)
	true
	false))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
 (let [name (get author :name)
	life (if (contains?  author :birth-year)
        (str " (" (get author :birth-year) " - " (get author :death-year) ")")
	(str ""))] 
(str name life)
))

(defn authors->string [authors]
 (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (get book :title) ", written by " (authors->string (get book :authors))))

(defn books->string [books]
(let [count (count books)
      stringi (str count " book"
		(if (> count 1) "s") ". " (apply str (interpose ", " (map book->string books)))".")]  
(if (== count 0)
"No books."
stringi)))

(defn books-by-author [author books]
 (let [auth (fn [book] (has-author? book author))] 
 (filter auth books)))

(defn author-by-name [name authors]
  (let [auth (fn [author] (= name (get author :name)))]
(first (filter auth authors))))

(defn living-authors [authors]
 (let [auth (fn [author] (alive? author))]
(filter auth authors)))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
