(ns structured-data)

(defn do-a-thing [x]
  (let [addme (+ x x)]
    (java.lang.Math/pow addme addme)))

(defn spiff [v] 
  (+ (get v 0) (get v 2))
  )


(defn cutify [v]
  (conj v "<3")
 )


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
      (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
  ))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle))
 )


(defn area [rectangle]
  (* (height rectangle) (width rectangle))
 )

 
(defn contains-point? [rectangle point]
  
 )

(defn contains-rectangle? [outer inner]
  :-)




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
      (assoc book :authors (conj (:authors book) new-author))
 )


(defn alive? [author]
   (not (contains? author :death-year)) 
 )


(defn element-lengths [collection]
  (map count collection)
 )


(defn second-elements [collection]
    (let [secondElem (fn [v] (second v))]
    (map secondElem collection))
)


(def books [cities, wild-seed, embassytown, little-schemer])

(defn titles [books]
  (map :title books)
  )

 

(defn stars [n]
     (apply str (apply concat(repeat n "*")))   
  )


(defn monotonic? [a-seq]
  :-)



(defn toggle [a-set elem]
  :-)

(defn contains-duplicates? [a-seq]
  :-)

(defn old-book->new-book [book]
  :-)

(defn has-author? [book author]
  :-)

(defn authors [books]
  :-)

(defn all-author-names [books]
  :-)

(defn author->string [author]
  :-)

(defn authors->string [authors]
  :-)

(defn book->string [book]
  :-)

(defn books->string [books]
  :-)

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
