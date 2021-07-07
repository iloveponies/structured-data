(ns structured-data)

(defn do-a-thing [x]
  (let [power (+ x x)]
  (Math/pow power power)))

(defn spiff [v]
   (+ (get v 0) (get v 2))) 

(defn cutify [v]
   (conj v "<3")) 

(defn spiff-destructuring [v]
  (let [[x _ y] v]
    (+ x y)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- x1 x2))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y1 y2))))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (and (= (height rectangle) (width rectangle)))))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (width rectangle) (height rectangle))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [p1 p2] point]
    (and (<= x1 p1 x2) (<= y1 p2 y2) true)))

(defn contains-rectangle? [outer inner]
  (let [[[xo1 yo1] [xo2 yo2]] outer [[xi1 yi1] [xi2 yi2]] inner]
    (and (<= xo1 xi1 xi2 xo2) (<= yo1 yi1 yi2 yo2))))

(defn title-length [book]
  (count (book :title)))

(defn author-count [book]
 (count (book :authors)))

(defn multiple-authors? [book]
  (if (> (count (book :authors)) 1) true false))

(defn add-author [book new-author]
  (let [{authors :authors} book
        all-authors (conj authors new-author)]
    (assoc book :authors all-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
 (let [takesecond (fn [collection] (second collection))]
    (map takesecond collection)))
  
(defn titles [books]
  (map :title books))
  
(defn monotonic? [a-seq]
  (if (apply <= (seq a-seq)) true
     (if (apply >= (seq a-seq)) true
       false)))
  
(defn stars [n]
  (apply str (repeat n "*")))
  
(defn toggle [a-set elem]
  (if(contains? a-set elem) (disj a-set elem) 
    (conj a-set elem)))
  
(defn contains-duplicates? [a-seq]
   (if (< (count (set a-seq) ) (count a-seq)) true false)) 

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))
    
(defn has-author? [book author]
  (if (contains? (book :authors) author) true 
    false))
  
(defn authors [books]
   (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set(map :name (authors books))))
  
(defn author->string [author]
  (let [name (author :name) 
        byear (author :birth-year) 
        dyear (author :death-year)]
    (str name (if (contains? author :birth-year) 
                (str " (" byear " - " dyear ")")))))
  
(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))
  
(defn book->string [book]
  (str (:title book) ", " "written by " (authors->string (:authors book))))
              
(defn books->string [books]
  (if (empty? books) "No books."
    (if (= (count books) 1) (str "1 book. " (book->string (books 0)) ".")
      (str (count books) " books. "(apply str (interpose ". " (map book->string books))) ".") )))
  
(defn books-by-author [author books]
  (filter (fn [book] (has-author? (old-book->new-book book) author)) books))
  
(defn author-by-name [name authors]
 (first (filter (fn [x] (= name (x :name))) authors)))
  
(defn living-authors [authors]
  (filter alive? authors))
  
(defn has-a-living-author? [book]
   (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))


; %________%
