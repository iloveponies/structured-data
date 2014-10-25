(ns structured-data)

(defn do-a-thing [x]
  (let [sum (+ x x)]
     (Math/pow sum sum)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[v0 _ v2] v]
    (+ v0 v2)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[ [blx bly] [trx try] ]]
  (- trx blx))

(defn height [[ [blx bly] [trx try] ]]
  (- try bly))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle) ))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[ [blx bly] [trx try] ] [px py]]
  (and (<= blx px trx) (<= bly py try)))

(defn contains-rectangle? [outer [bl tr]]
  (and (contains-point? outer bl) (contains-point? outer tr)))


(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authorf (:authors book)]
    (assoc book :authors (conj authorf new-author) )))

(defn alive? [author]
  (not (:death-year author)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn [coll] (get coll 1))]
    (map get-second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(use 'clojure.set)

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [namel (:name author)
        deathl (:death-year author)
        birthl (:birth-year author)]
    (str namel (if birthl (str " (" birthl " - " deathl ")")))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [nbooks (count books)
        booktext (fn [bookf] (str (book->string bookf) "."))
        booklist (apply str (interpose " " (map booktext books)))]
    (if (= 0 nbooks) "No books." (str nbooks " book" (if (> nbooks 1) "s") ". " booklist))))

(defn books-by-author [author books]
  (filter (fn [bookf] (has-author? bookf author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [authorf] (= name (:name authorf))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
