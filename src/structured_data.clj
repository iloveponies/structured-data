(ns structured-data)

(defn do-a-thing [x] 
  (let [k (+ x x)] 
	(Math/pow k k ))
  )

(defn spiff [v]
	(+ (get v 0) (get v 2))
  )

(defn cutify [v]
	(conj v "<3")   
  )

(defn spiff-destructuring [v]
	(let [[x y z] v]
  (+ x z )
  ))

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
		(- x2 x1) (- y2 y1)
      )
  )

(defn square? [rectangle]
	(let [[[x1 y1] [x2 y2]] rectangle]
		(if(= (- x1 x2) (- y1 y2) ) true false
      ))
  )

(defn area [rectangle]
	(let [[[x1 y1] [x2 y2]] rectangle]
		(* (height rectangle) (width rectangle))
      )
  )

(defn contains-point? [rectangle point]
  :-)

(defn contains-rectangle? [outer inner]
  :-)

(defn title-length [book]
	(count(:title book))
  )

(defn author-count [book]
   (count(:authors book))
  )

(defn multiple-authors? [book]
	(if (> (author-count book) 1) true false)
  )

(defn add-author [book new-author]
  (let [authors (get book :authors)]
    (assoc book :authors (conj authors new-author))
    )
  )

(defn alive? [author]
	(if (get author :death-year) false true)
  )

(defn element-lengths [collection]
  (let [laske (fn [x] (count x))]
    (map laske collection)))

(defn second-elements [collection]
  (let [laske (fn [x] (get x 1))]
    (map laske collection)))

(defn titles [books]
	(map :title books)
  )

(defn monotonic? [a-seq]
  :-)

(defn stars [n]
	(apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
	(if (contains? a-set elem)
       (disj a-set elem)
       (conj a-set elem))
  )

(defn contains-duplicates? [a-seq]
  (if 
    (= (count a-seq) (count (set a-seq))) 
     false 
     true )
  )

(defn old-book->new-book [book]
  (let [authors (get book :authors)]
    (assoc book :authors (set authors))
  )
)

(defn has-author? [book author]
      (let [authors (get book :authors)]
			(contains? authors author)
        )
  	)

(defn authors [books]
	(let [authors (map :authors books)]
    (apply clojure.set/union authors )
		)
  )

(defn all-author-names [books]
	(let [authors (authors books)] 
      (set (map :name authors))
      )
  )

(defn author->string [author]
  (let [name (get author :name) 
        birth (get author :birth-year) 
        death (get author :death-year)]
    	(if (or birth death)
			(str name " (" birth " - " death")")
          	(str name )
          )
    )
  )

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