(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)
   )
 )


(defn spiff [v]
  (let [a (get v 0)
        b (get v 2)]
    (+ a b)
   )
 )

(defn cutify [v]
  (conj v "<3")
 )

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)
   )
 )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (- x2 x1)
   )
 )

(defn height [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (- y2 y1)
   )
 )

(defn square? [rectangle]
  (= (height rectangle) (width rectangle))
 )

(defn area [rectangle]
  (* (height rectangle) (width rectangle))
 )

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle
        [x y] point]
    (and (<= x1 x x2) (<= y1 y y2))
   )
 )

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))
   )
 )

(defn title-length [book]
  (count (:title book))
 )

(defn author-count [book]
  (count (:authors book))
 )

(defn multiple-authors? [book]
  (< 1 (author-count book))
 )

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))
   )
 )

(defn alive? [author]
  (not (contains? author :death-year))
 )

(defn element-lengths [collection]
  (map count collection)
 )

(defn second-elements [collection]
  (let [getSecond (fn [x] (get x 1))]
    (map getSecond collection)
   )
 )

(defn titles [books]
  (map :title books)
 )

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
  (not= (count (set a-seq)) (count a-seq))
 )

(defn old-book->new-book [book]
  (let [authors (:authors book)
        newAuthors (set authors)]
    (assoc book :authors newAuthors )
   )
 )

(defn has-author? [book author]
  (contains? (:authors book) author)
 )

(defn authors [books]
  (apply clojure.set/union (map :authors books))
 )

(defn all-author-names [books]
  (set (map :name (authors books)))
 )

(defn author->string [author]
  (let[name (:name author)
       birth (:birth-year author)
       death (:death-year author)]
     (cond
       (not (contains? author :birth-year)) (str name)
       :else (str name " (" birth " - " death ")")
      )
   )
 )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
 )

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book)))
 )

(defn books->string [books]
  (cond
    (empty? books) (str "No books.")
    (= 1 (count books)) (str "1 book. " (apply str (interpose ". " (map book->string books))) ".")
    :else (str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")
   )
 )

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
  (cond
    (empty? (living-authors (:authors book))) false
    :else true
   )
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
 )

; %________%
