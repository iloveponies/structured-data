(ns structured-data)

(defn do-a-thing [x]
  (let 
    [doublex (+ x x)] 
    (Math/pow doublex doublex)))

(defn spiff [v]
  (let 
    [first (first v)
     third (get v 2)]
    (+ first third)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[fst _ trd]]
  (+ fst trd))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[blx bly] [trx try]]]
  (- trx blx))

(defn height [[[blx bly] [trx try]]]
  (- try bly))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[rblx rbly] [rtrx rtry]] [px py]]
  (and 
    (>= px rblx)
    (<= px rtrx)
    (>= py rbly)
    (<= py rtry)))

(defn contains-rectangle? [outer inner]
  (let [[ibl itr] inner]
  (and 
    (contains-point? outer ibl)
    (contains-point? outer itr))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors
    (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map (fn [c] (get c 1)) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply <= (map - a-seq))))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
	 (disj a-set elem)
	 (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
 (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
	(let [
		n (:name author)
		d (:death-year author)
		b (:birth-year author)
		yrs (if b (str " " \( b " - " d \)))]
		(str n yrs)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
 (let [
	cnt (count books)
	cntstr 
		(if (== 0 cnt) "No books"
		 (if (== 1 cnt) "1 book. "
			(str cnt " books. ")))
	booksstr (apply str (interpose ". " (map book->string books)))]
		(str cntstr booksstr ".")))

(defn books-by-author [author books]
  (filter (fn [b] (has-author? b author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [a] (= (:name a) name)) authors)))

(defn living-authors [authors]
	(filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
