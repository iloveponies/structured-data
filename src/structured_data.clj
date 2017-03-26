(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
	(Math/pow xx xx	)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
 (let [[ x y z] v]
	(+ z x)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[ x1 y1 ][x2 y2]] rectangle]
	(- x2 x1)))

(defn height [rectangle]
  (let [[[ x1 y1 ][x2 y2]] rectangle]
	(- y2 y1)))

(defn square? [rectangle]
	(if (= (width rectangle) (height rectangle))
	true
	false))

(defn area [rectangle]
	(* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[ x1 y1 ][x2 y2]] rectangle
        [x3 y3] point]
	(and (>= x2 x3 x1) (>= y2 y3 y1))))

(defn contains-rectangle? [outer inner]
  (let [[[ x1 y1 ][x2 y2]] outer
        [[ x3 y3 ][x4 y4]] inner]
	(and (>= x2 x4 x3 x1) (>= y2 y4 y3 y1))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (count (:authors book)) 1)
	true
	false))
  

(defn add-author [book new-author]
	(let
	[ new-authors (conj (:authors book) new-author) ]
	(assoc book :authors new-authors)))

(defn alive? [author]
	(if (:death-year author)
	false
	true))

(defn element-lengths [collection]
	(map count collection))

(defn second-elements [collection]
	(let [seconds (fn [co] (get co 1))]
	(map seconds collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
     (if (apply >= a-seq)
	true
	(if (apply <= a-seq)
		true
		false)))
(defn stars [n]
	(let [ asteriks (repeat n "*")]
	(apply str asteriks)))

(defn toggle [a-set elem]
	(if (contains? a-set elem)
		(disj a-set elem)
		(conj a-set elem)))

(defn contains-duplicates? [a-seq]
	(let [ a-set (set a-seq)]
		(if (= (count a-set) (count a-seq))
		false
		true)))

(defn old-book->new-book [book]
	(let [new-authors (set (:authors book))]
	(assoc book :authors new-authors)))

(defn has-author? [book author]
	(contains? (:authors book) author))

(defn authors [books]
	(apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
	(set (apply clojure.set/union (conj #{} (map :name (authors books))))))


(defn author->string [author]
	(let [author-name (:name author)
	      author-birth (:birth-year author)
	      author-death (:death-year author)]
	(cond 
	   author-death (str author-name " (" author-birth " - " author-death ")")
	   author-birth (str author-name " (" author-birth " - )")
	   :else (str author-name))))
		

(defn authors->string [authors]
(apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
	(let [authors (:authors book)
	      book-title (:title book)
	      ]
	(cond 
	   authors (str book-title ", written by " (authors->string authors))
	   :else (str book-title))))

(defn books->string [books]
	(cond
		(empty? books) (str "No books.")
		(= 1 (count books)) (str "1 book. " (book->string (get books 0)) ".")
		:else (str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")))

(defn books-by-author [author books]
	(filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
	(let [names (filter (fn [x] (= name (:name x))) authors)]
	(first names)))

(defn living-authors [authors]
	(filter alive? authors))

(defn has-a-living-author? [book]
	(let [x (living-authors (:authors book))]
 		(if (empty? x)
			false
			true)))


(defn books-by-living-authors [books]
	(filter has-a-living-author? books))

; %________%
