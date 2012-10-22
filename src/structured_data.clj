(ns structured-data)

(defn do-a-thing [x]
  (let [ x2 (+ x x)]
	(Math/pow x2 x2)
    ))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
	(+ a c)

    ))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [ [p1 p2] rectangle [x1 y1] p1 [x2 y2] p2 ]
	(- x2 x1)
  ))

(defn height [rectangle]
  (let[ [p1 p2] rectangle [x1 y1] p1 [x2 y2] p2 ]
	(- y2 y1)
  ))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [ [p1 p2] rectangle [x1 y1] p1 [x2 y2] p2 [a b] point]
  	(and 
		(<= x1 a x2)
		(<= y1 b y2)
   	)

  ))

(defn contains-rectangle? [outer inner]
  (let [ [p1 p2] inner ]
	(and
     (contains-point? outer p1)
     (contains-point? outer p2)
     )
    
    ))

(defn title-length [book]
  (let [title (:title book)]
	(count title)

    ))

(defn author-count [book]
  (let [authors (:authors book)]
	(count authors)

   ))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
	(assoc book :authors (conj (:authors book) new-author))
  )

(defn alive? [author]
  (not (:death-year author)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map (fn[v](first (rest v))) collection ))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or 
	(apply <= a-seq)
   	(apply >= a-seq)
   ))

(defn stars [n]
   (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
	(disj a-set elem)
    (conj a-set elem)
    ))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [title (:title book) authors (:authors book)]
	{:title title, :authors (set authors)}
    ))

(defn has-author? [book author]
  (let [authors (:authors (old-book->new-book book))]
	(contains? authors author)
    ))

(defn authors [books]
  (apply clojure.set/union (map :authors (map old-book->new-book books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [n (:name author) b (:birth-year author) d (:death-year author)
	life (cond d (str " (" b " - " d ")")
			b (str " (" b " - " ")")
			:else "")]

    (str n life)
    ))

(defn authors->string [authors]
  (apply str (interpose ", "(map author->string authors))))

(defn book->string [book]
  (let [t (:title book) a (:authors book)]
	(str t ", written by " (authors->string a))
    ))

(defn books->string [books]
  (let [c (count books)
		bookcount (if (= c 1) (str c " book. ") (str c " books. ") )
		bookstrings (apply str (interpose ". " (map book->string books)))
        ]
	(if (= c 0)
	"No books."
	(str bookcount bookstrings ".")
    )))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books ))