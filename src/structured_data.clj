(ns structured-data)

(defn do-a-thing [x]
	(let [xx (+ x x)]
	 (Math/pow xx xx))
) ;1

(defn spiff [v]
	(+ (get v 0 ) (get v 2))
) ;2

(defn cutify [v]
	(conj v "<3") 
) ;3

(defn spiff-destructuring [v]
	(let [s (spiff v)]
	s)
) ;4

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
 	(let [[[x1 y1] [x2 y2]] rectangle]
  	(- x2 x1))

) ;5

(defn height [rectangle]
	(let [[[x1 y1] [x2 y2]] rectangle]
  	(- y2 y1))

)

(defn square? [rectangle]
	(if (= (width rectangle) (height rectangle)) true false)
) ;6

(defn area [rectangle]
	(* (width rectangle) (height rectangle))
) ;7

(defn contains-point? [rectangle point]
	(let [[x3 y3] point] 
	(let [[[x1 y1] [x2 y2]] rectangle ]
	(if  (and (>= x3 x1) (<= x3 x2) (>= y3 y1) (<= y3 y2)) true false)
	))
) ;8

(defn contains-rectangle? [outer inner]
	(let [[[x1 y1] [x2 y2]] inner ]
	(if (and (contains-point? outer [x1,y1]) 
	     	 (contains-point? outer [x2,y2])) true false)
	)
) ;9


(defn title-length [book]
	(count (get book :title))
) ;10

(defn author-count [book]
	(count (book :authors))
) ;11

(defn multiple-authors? [book]
	(if (> (author-count book) 1) true false)
) ;12

(defn add-author [book new-author]
	(let [original (book :authors)
      	new (conj original new-author)]	
	(assoc book :authors new)	
	)
) ;13

(defn alive? [author]
	(not (contains? author :death-year))
) ;14

(defn element-lengths [collection]
	(map count collection) 
) ;15

(defn second-elements [collection]
  (let [sec (fn [x] (get x 1))]
    (map sec collection)) 
) ;16

(defn titles [books]
	(let [b books]
  	(map :title b))
) ;17

(defn monotonic? [a-seq]
	(or (apply <= a-seq) (apply >= a-seq))
) ;18

(defn stars [n]
	(apply str (repeat n "*"))
) ;19

(defn toggle [a-set elem]
	(if (contains? a-set elem)
		(disj a-set elem)
		(conj a-set elem)
	)
) ;20

(defn contains-duplicates? [a-seq]
	(< (count (distinct a-seq)) (count  a-seq))
) ;21

(defn old-book->new-book [book]
	 (assoc book :authors (set (:authors book)))
) ;22

(defn has-author? [book author]
	(contains? (book :authors) author) 
) ;23

(defn authors [books]
  (let [author-names
         (fn [book] map (:authors book))]
	(set (apply clojure.set/union (map author-names books))))
) ;24

(defn all-author-names [books] 
	 (let [author-names
         (fn [book] (map :name (authors books)))]
   	 (set (apply clojure.set/union (map author-names books))))
) ;25

(defn author->string [author]
	(cond
	 (contains? author :death-year) (str (author :name) " (" (author :birth-year) " - " (author :death-year) ")" )
	 (contains? author :birth-year) (str (author :name) " (" (author :birth-year) " - " ")" )
	:else (str (author :name)) )
) ;26

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
