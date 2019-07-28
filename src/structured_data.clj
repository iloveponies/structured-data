(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0)
     (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[x y z]]
  (+ x z))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (== (height rectangle)
      (width rectangle)))

(defn area [rectangle]
  (* (width rectangle)
     (height rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [x y]]
  (and (<= x1 x x2)
       (<= y1 y y2)))

(defn contains-rectangle? [outer [top-left bottom-right]]
  (and (contains-point? outer top-left)
       (contains-point? outer bottom-right)))

(defn title-length [book]
  (count (:title book "")))

(defn author-count [book]
  (count (:authors book [])))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book [])]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map (fn [v] (get v 1)) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  ((if (contains? a-set elem)
      disj
      conj) a-set elem))

(defn contains-duplicates? [a-seq]
  (not (== (count (set a-seq))
           (count a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors
         (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)
        life (if (not (nil? birth-year))
               (str " (" birth-year " - " death-year ")"))]
    (str name life)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) (if (not (empty? (:authors book)))
                       (str ", written by "
                            (authors->string (:authors book))))))

(defn books->string [books]
  (let [book-count (count books)
        header (cond
                (== book-count 0) "No books."
                (== book-count 1) "1 book."
                :else (str book-count " books."))]
    (if (== book-count 0)
      header
      (str header " "
           (apply str
                  (interpose ". " (map book->string books)))
           "."))))


(defn books-by-author [author books]
  (filter (fn [book]
            (has-author? book author))
          books))

(defn author-by-name [name authors]
  (first (filter (fn [author]
                   (= (:name author)
                      name))
                 authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (> (count (living-authors (:authors book)))
     0))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

