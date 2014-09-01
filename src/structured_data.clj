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
  :-)

(defn element-lengths [collection]
  :-)

(defn second-elements [collection]
  :-)

(defn titles [books]
  :-)

(defn monotonic? [a-seq]
  :-)

(defn stars [n]
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
