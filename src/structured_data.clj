(ns structured-data)

(defn do-a-thing [x]
  (let [x+x (+ x x)]
    (Math/pow x+x x+x) ) )

(defn spiff [v]
  "adds the first and third element of v - returns nil if v is too short"
  (if (< (count v) 3)
      nil
	  (+ (get v 0) (get v 2)) ) )

(defn cutify [v]
  (conj v "<3") )

(defn spiff-destructuring [v]
  "adds the first and third element of v - returns nil if v is too short"
  (if (< (count v) 3)
      nil
     (let [[x _ y] v]
       (+ x y) ) ) )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1) ) )

(defn height [rectangle]
    (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1) ) )

(defn square? [rectangle]
    (= (width rectangle) (height rectangle) ) )

(defn area [rectangle]
  (* (width rectangle) (height rectangle)) )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
		[xp yp] point ]
	(and (<= x1 xp x2) (<= y1 yp y2)) ) )

(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
    (and (contains-point? outer bottom-left)
		 (contains-point? outer top-right) ) ) )

(defn title-length [book]
  (count (:title book)) )

(defn author-count [book]
  (count (:authors book)) )

(defn multiple-authors? [book]
  ( > (author-count book) 1) )

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author)) ) )

(defn alive? [author]
  (not (contains? author :death-year)) )

(defn element-lengths [collection]
  (map count collection) )

(defn second-elements [collection]
  (map (fn [vec] (get vec 1)) collection) )

(defn titles [books]
  (map :title books) )

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq) ) )

(defn stars [n]
  (apply str (repeat n "*")) )

(defn toggle [a-set elem]
  ((if (contains? a-set elem) disj conj) a-set elem) )

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))) )

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))) )

(defn has-author? [book author]
  (contains? (:authors book) author) )

(defn authors [books]
  (apply clojure.set/union
        (map :authors books) ) )

(defn all-author-names [books]
  (set (map :name (authors books))) )
  
(defn author->string [author]
  (let [years->string (fn [birth-year death-year]
						(if (not birth-year)
							 ""
						    (str " (" birth-year " - " death-year ")") ) ) ]
	(str (:name author)
	     (years->string (:birth-year author) (:death-year author)) ) ) )
				  
(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))) )

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))) )

(defn books->string [books]
  (let [num-books (count books)
		op-string (cond
				    (= num-books 0) "No books"
					(= num-books 1) "1 book. "
					:else (str num-books " books. ") )
        book-strings (interpose ". " (map book->string books)) ]
    (str op-string
		 (apply str book-strings)
		  "." ) ) )
		

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books) )

(defn author-by-name [name authors]
  (letfn [(h [authors]
			(cond
			 (empty? authors) nil
			 (= (:name (first authors)) name) (first authors)
			 :else (h (rest authors)) ) )]
	(h authors) ) )

(defn living-authors [authors]
  (filter alive? authors) )

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))) )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books) )

; %________%
