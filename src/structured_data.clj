(ns structured-data)

(defn do-a-thing [x]
  (let [twiceX (+ x x)]
    (Math/pow twiceX twiceX)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let  [[x y z ] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [ [ [x1 y1] [x2 y2] ]  rectangle]
    (if (< (- x2 x1) 0) (- x1 x2) (- x2 x1)))
  )

(defn height [rectangle]
  (let [ [ [x1 y1] [x2 y2] ] rectangle]
    (if ( < (- y2 y1) 0) (- y1 y2) (- y2 y1))
    ))

(defn square? [rectangle]
  (if (== (width rectangle) (height rectangle)) true false ))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [ [ [x1 y1] [x2 y2] ]  rectangle
         [x3 y3] point]
    (and (<= x1 x3 x2) (<= y1 y3 y2))
      )
  )

(defn contains-rectangle? [outer inner]
  (let [ [p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))
      ))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  ( if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author)))
  )

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map (fn [x] (count x)) collection))

(defn second-elements [collection]
  (map (fn [x] (get x 1)) collection))

(defn titles [books]
  (map (fn [x] (:title x)) books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (== (count a-seq) (count (set a-seq))) false true)
  )

(defn old-book->new-book [book]
  (let [authors (:authors book)]
    (assoc book :authors (set authors))
    )
  )

(defn has-author? [book author]
  (contains?  (:authors book) author))

(defn authors [books]
  (let [writers (fn [book] (:authors book))]
    (apply clojure.set/union (map writers books)))
  )

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (let [name (:name author)
        bdate (:birth-year author)
        ddate (:death-year author)]
    (cond
     (not (nil? ddate)) (str name " (" bdate " - " ddate ")")
     (not (nil? bdate)) (str name " (" bdate " - )")
     :else (str name) 
     )
 ))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  ) 

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book)))
  )

(defn books->string [books]
  (cond
   (empty? books) "No books."
   :else (let
             [count (if (== (count books) 1) "1 book." (str (count books) " books."))]
           (str count " " (apply str (interpose ". " (map book->string books))) ".")
          )
   )
  )
(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)
  )

(defn author-by-name [name authors]
  (let [res (filter (fn [x] (= name (:name x))) authors)]
    ( if (empty? res) nil (first res)))
  )

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [authors (:authors book)
        live-authors (filter alive? authors)]
    (if (empty? live-authors) false true))
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
