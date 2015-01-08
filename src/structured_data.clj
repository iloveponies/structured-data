(ns structured-data)


(defn do-a-thing [x]
  (let [z (+ x x)]
  (Math/pow z z))
  )

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

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
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let[[[x1 y1] [x2 y2]] rectangle
       [x3 y3] point]
    (and (<= x1 x3 x2) (<= y1 y3 y2))))


(defn contains-rectangle? [outer inner]
  (let[[p1 p2] inner]
    (and (contains-point? outer p1)
         (contains-point? outer p2))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (book :authors)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))


(defn second-elements [collection]
  (let [second (fn [n] (get n 1))]
    (map second collection)))

(second-elements [[1 2] [2 3] [3 4]])

(defn titles [books]
  (let [getTitle (fn [book] (:title book))]
    (map getTitle books)))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))


(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))


(defn contains-duplicates? [a-seq]
  (not (== (count (set a-seq)) (count a-seq))))

(defn old-book->new-book [book]
  (let [a-set (set (book :authors))]
    (assoc book :authors a-set)))

(defn has-author? [book author]
  (let [authors (book :authors)]
    (contains? authors author)
    ))


(defn authors [books]
  (apply clojure.set/union (map (fn [book] (get book :authors)) books)))


(defn all-author-names [books]
  (let [ba (authors books)
        getAuthorName (fn [author] (get author :name))]
    (set (map getAuthorName ba))))

(defn author->string [author]
  (cond
   (contains? author :death-year) (str (author :name) " (" (author :birth-year) " - " (author :death-year) ")")
   (contains? author :birth-year) (str (author :name) " (" (author :birth-year) " - )")
  :else (author :name)))

(defn authors->string [authors]
  (cond
   (= (count authors) 0)
   ""
   (> (count authors) 1)
     (str (author->string (first authors)) ", " (authors->string (rest authors)))
  :else
   (author->string (first authors))
    ))

(defn book->string [book]
  (str (book :title) ", written by " (authors->string (book :authors))))

(defn books->string [books]
  (let [bc (count books)
        bookList (apply str (map (fn [book] (str " " (book->string book) ".")) books))]
        (if (= 0 bc)
          "No books."
          (if (> bc 1)
            (str bc ". books." bookList)
            (str "1 book." bookList)))))


(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))


(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (author :name))) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (book :authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
