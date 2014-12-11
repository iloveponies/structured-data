(ns structured-data)

(defn do-a-thing [x]
;	(Math/pow (+ x x) (+ x x))
	(let [xx (+ x x)]
	(Math/pow xx xx))
	)

(defn spiff [v]
  (+ (get v 0) (get v 2))
  )

(defn cutify [v]
  (conj v ">3")
  )

(defn spiff-destructuring [v]
  (let [[a b] [(get v 0) (get v 2)]]
  (+ a b))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (- x2 x1))
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (- y2 y1))
	)
(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (== (- x2 x1) (- y2 y1)))
  )

(defn area [rectangle]
	(let [[[x1 y1] [x2 y2]] rectangle]
	(* (height rectangle) (width rectangle)))
  )

(defn contains-point? [rectangle point]
  (let 
	[
		[[x1 y1] [x2 y2]] rectangle
		[px py] point
	]
	(and (< x1 px x2) (< y1 py y2))
	)
  )

(defn contains-rectangle? [outer inner]
  (let 
	[
		[[x1 y1] [x2 y2]] inner
	]

	(and
		(contains-point? outer [x1 y1])
		(contains-point? outer [x2 y2]))
	)
  )

(def china {:name "China Miéville", :birth-year 1972})
(def octavia {:name "Octavia E. Butler"
              :birth-year 1947
              :death-year 2006})
(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})

(def cities {:title "The City and the City" :authors #{china}})
(def wild-seed {:title "Wild Seed", :authors #{octavia}})
(def embassytown {:title "Embassytown", :authors #{china}})
(def little-schemer {:title "The Little Schemer"
                     :authors #{friedman, felleisen}})

(def books [cities, wild-seed, embassytown, little-schemer])
  
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
  (= nil (:death-year author))
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [second (fn [x] (get x 1))]
	(map second collection)
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
  (conj a-set elem))
  )

(defn contains-duplicates? [a-seq]
  (not(= (count a-seq) (count (set a-seq))))
  )

(defn old-book->new-book [book]
  ;;use assoc to turn book with authors in sequence into autors in set
  (assoc book :authors (set(:authors book)))
  )

;(assoc a-map a-key a-value) sets the value of a-key in a-map to be a-value.
;(assoc {:a 1} :b 2) ;=> {:b 2, :a 1}
;(assoc {:a 1} :a 2) ;=> {:a 2}
;(old-book->new-book {:title "The Little Schemer" :authors [friedman, felleisen]})
  
(defn has-author? [book author]
  (contains? (:authors book) author)
  )

(defn all-author-names [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))
	
;	(let ;
;	[
;		[[x1 y1] [x2 y2]] rectangle
;		[px py] point
;	]
;	(and (< x1 px x2) (< y1 py y2))
;	)

(defn author->string [author]
	(let [
		name (:name author)
		years 
		(if
			(= nil (:birth-year author)) 
			""	
			(str " (" (:birth-year author) "-" (:death-year author) ")")
			)
		]
		(str name years)
  )
 )
 
 (defn authors->string [authors]
	(apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
	(str (:title book) ", written by " (authors->string (:authors book)))
)

;"3 books. 
;The Little Schemer, written by Daniel Friedman (1944 - ), Matthias Felleisen. 
;The City and the City, written by China Miéville (1972 - ). 
;Wild Seed, written by Octavia E. Butler (1947 - 2006)."

(defn books->stringx [books]
  (str
	(cond
		(== 0 (count books)) "No books."
		(== 1 (count books)) "1 book. "
		:else (str (count books) " books. ")
		)
;	(apply str (count books) " book" (if (> (count books) 1) "s. " ". ") (interpose ". " (map book->string books)))
		(apply str (interpose ". " (map book->string books)))
		)
  )


(defn books->stringy [books]
  (let 
  [
	countstring 
		(cond
			(== 0 (count books)) "No books."
			(== 1 (count books)) "1 book. "
			:else (str (count books) " books. ")
			)
	]
	(apply str countstring (interpose ". " (map book->string books)))
	
	)
)

  
(defn books->string [books]
  (let 
	[bookstring (apply str (interpose ". " (map book->string books)))]
		(cond
			(== 0 (count books)) (str "No books.")
			(== 1 (count books)) (str "1 book. "  bookstring)
			:else (str (count books) " books. " bookstring)
			)
	)
)

(defn books-by-author [author books]
	(first (filter (fn [x] (has-author? x author)) books))
  )

(defn author-by-name [name authors]
  (filter (fn [author] (= (:name author) name)) authors)
  )

(defn living-authors [authors]
  (filter alive? authors)
  )

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))
  )

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books)
  )

; %________%
