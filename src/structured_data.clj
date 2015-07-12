(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
     (Math/pow xx xx)))

(defn spiff [v]
  (let [a (get v 0)
        b (get v 2)]
      (+ a b )))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[a b c]]
  (+ a c))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[rx1 ry1] [rx2 ry2]] rectangle
        [px py] point]
      (and (<= rx1 px rx2)
           (<= ry1 py ry2))))

(defn contains-rectangle? [outer [innerBottomLeft innerTopRight]]
  (and (contains-point? outer innerBottomLeft)
       (contains-point? outer innerTopRight)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)
        new-authors (conj authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [getSecond (fn [v] (get v 1))]
    (map getSecond collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (let [makeStars (repeat n "*")]
    (apply str makeStars)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) 
        (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [seqOfAuthors (map :authors books)] 
    (apply clojure.set/union seqOfAuthors)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        years (str "(" 
                     (:birth-year author) 
                     " - " 
                     (:death-year author)
                     ")")]
    (if (contains? author :birth-year)
      (str name " " years)
      (str name))))

(defn authors->string [authors]
  (let [authorStrings (map author->string authors)]
    (apply str (interpose ", " authorStrings))))

(defn book->string [book]
  (str (:title book)
       ", written by "
       (authors->string (:authors book))))

(defn books->string [books]
  (let [bookCount (if (> (count books) 1)
                    (str (count books) " books. ")
                    (str (count books) " book. "))
       bookStrings (map book->string books)
       bookString (interpose ". " bookStrings)]
    (if (empty? books)
      "No books."
      (str bookCount (apply str bookString) "."))))

(defn books-by-author [author books]
  (filter (fn [b] (has-author? b author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [a] (= name (:name a))) authors)))

(defn living-authors [authors]
  (filter (fn [a] (alive? a)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [b] (has-a-living-author? b)) books))

; %________%
