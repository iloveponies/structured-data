(ns structured-data)

(defn do-a-thing [x]
  (let
    [p (+ x x)]
    (Math/pow p p)
    )
  )

(defn spiff [v]
  (+ (get v 0) (get v 2))
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
  (let[[[x1 y1] [x2 y2]] rectangle]
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
    (cond
      (and (== (- x1 y1) (- x2 y2))) true
      :else false
     )
    )
  )

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (* (- x2 x1) (- y2 y1)))
   )
  )

(defn contains-point? [rectangle point]
   (let [[[x1 y1] [x2 y2]] rectangle [px py] point]
    (cond
      (and (<= x1 px x2) (<= y1 py y2)) true
      :else false
     )
    )
  )

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer [[xi1 yi1] [xi2 yi2]] inner]
    (cond
      (and
       (<= x1 xi1 x2)
       (<= x1 xi2 x2)
       (<= y1 yi1 y2)
       (<= y1 yi2 y2)
      ) true
      :else false
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
  (cond
   (> (count (:authors book)) 1) true
   :else false
   ))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author))
  )

(defn alive? [author]
  (cond
   (contains? author :death-year) false
   :else true
   )
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [seconds (fn [x](second x))]
    (map seconds collection))
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
  (cond
   (contains? a-set elem) (disj a-set elem)
   :else (conj a-set elem)
   )
  )

(defn contains-duplicates? [a-seq]
  (cond
   (== (count a-seq) (count (set a-seq))) false
   :else true
   )
  )

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
  )

(defn has-author? [book author]
   (contains? (:authors book) author)
  )

(defn authors [books]
   (apply clojure.set/union (map :authors books))
  )

(defn all-author-names [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books))))
  )

(defn author->string [author]
  (str (:name author) (if (contains? author :birth-year) (str " (" (:birth-year author) " - " (:death-year author) ")") ""))
  )

(defn authors->string [authors]
   (apply str
      (interpose ", " (map author->string authors))
    )
  )

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book)))
  )

(defn books->string [books]
  (cond
   (empty? books) "No books."
   :else
     (str (count books)(if (> (count books) 1) " books." " book.") " " (apply str (interpose ". "(map book->string books))) ".")
   )
  )

(defn books-by-author [author books]
  (filter (fn [x] (contains? (:authors x) author)) books)
  )

(defn author-by-name [name authors]
  (let[x (filterv (fn [x] (= (:name x) name)) authors)]
    (if (empty? x) nil (first x))
  )
  )

(defn living-authors [authors]
  (filter (fn [x] (not (contains? x :death-year))) authors)
  )

(defn has-a-living-author? [book]
  (if (empty? (living-authors (:authors book))) false true)
  )

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books)
  )

; %________%
