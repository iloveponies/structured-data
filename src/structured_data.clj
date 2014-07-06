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


(def china {:name "China Miéville", :birth-year 1972})
(def octavia {:name "Octavia E. Butler"
              :birth-year 1947
              :death-year 2006})
(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})

(def cities {:title "The City and the City" :authors [china]})
(def wild-seed {:title "Wild Seed", :authors [octavia]})
(def embassytown {:title "Embassytown", :authors [china]})
(def little-schemer {:title "The Little Schemer"
                     :authors [friedman, felleisen]})


(defn title-length [book]
   (count (:title book))  
 )

(title-length cities)  
(title-length wild-seed)  
(title-length little-schemer)

(defn author-count [book]
  (count (:authors book))
 )
(author-count little-schemer)

(defn multiple-authors? [book]
  (> (author-count book) 1)
 )

(multiple-authors? little-schemer)   

(assoc cities :awards ["Hugo", "World Fantasy Award",
                       "Arthur C. Clarke Award",
                       "British Science Fiction Award"])

(defn add-author [book new-author]
      (assoc book :authors (conj (:authors book) new-author))
 )

(add-author little-schemer {:name "Gerald J. Sussman"})
(add-author {:authors [{:name "Juhana"}]} {:name "Jani"})

(defn alive? [author]
   (not (contains? author :death-year)) 
 )

(alive? octavia)  

(defn element-lengths [collection]
  (map count collection)
 )
(element-lengths ["foo" "bar" "" "quux"])
(element-lengths ["x" [:a :b :c] {:y 42}]) 

(defn second-elements [collection]
    (let [secondElem (fn [v] (second v))]
    (map secondElem collection))
)

(second-elements [[1 2] [2 3] [3 4]])
(second-elements [[1 2 3 4] [1] ["a" "s" "d" "f"]])

(def books [cities, wild-seed, embassytown, little-schemer])

(defn titles [books]
  (map :title books)
  )
(titles [cities])
(titles books)

 

(defn stars [n]
     (apply str (apply concat(repeat n "*")))   
  )

(stars 5)

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
